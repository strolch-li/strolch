package li.strolch.service.notifications;

import com.google.gson.JsonObject;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.builder.ResourceBuilder;
import li.strolch.model.json.FromFlatJsonVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.SupportedLanguage;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

import java.util.Set;
import java.util.stream.Collectors;

import static li.strolch.model.StrolchModelConstants.*;

public class CreateNotificationService extends AbstractService<JsonServiceArgument, ServiceResult> {
	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public JsonServiceArgument getArgumentInstance() {
		return new JsonServiceArgument();
	}

	@Override
	protected ServiceResult internalDoService(JsonServiceArgument arg) throws Exception {
		DBC.PRE.assertNotNull("JsonElement must be set", arg.jsonElement);
		DBC.PRE.assertNotNull("JsonElement must be a JsonObject", arg.jsonElement.isJsonObject());

		JsonObject jsonObject = arg.jsonElement.getAsJsonObject();
		Resource notification = buildNotification(jsonObject, getSupportedLanguages(getAgent()));

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			tx.add(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	protected static Resource buildNotification(JsonObject jsonObject, Set<String> supportedLanguages) {
		DBC.PRE.assertTrue("JsonObject must have languages!", jsonObject.has(PARAM_LANGUAGES));
		JsonObject languagesJ = jsonObject.get(PARAM_LANGUAGES).getAsJsonObject();
		DBC.PRE.assertNotEmpty("JsonObject must have at least one languages!", languagesJ.keySet());

		FromFlatJsonVisitor visitor = new FromFlatJsonVisitor(jsonObject);
		visitor.optionalParameter(PARAM_ROLES);
		visitor.optionalParameter(PARAM_LOCATIONS);
		visitor.optionalParameter(PARAM_FOR_ALL);
		Resource notification = newNotification();
		notification.accept(visitor);

		for (String language : languagesJ.keySet()) {
			if (!supportedLanguages.contains(language))
				throw new IllegalArgumentException("The agent doesn't support language " + language);
			JsonObject languageJ = languagesJ.get(language).getAsJsonObject();
			String title = languageJ.get(PARAM_TITLE).getAsString();
			String text = languageJ.get(PARAM_TEXT).getAsString();

			ParameterBag languageBag = new ParameterBag(language, language, TYPE_TEXT);
			languageBag.setString(PARAM_TITLE, title);
			languageBag.setString(PARAM_TEXT, text);
			notification.addParameterBag(languageBag);
		}
		return notification;
	}

	protected static Set<String> getSupportedLanguages(StrolchAgent agent) {
		return agent
				.getStrolchConfiguration()
				.getRuntimeConfiguration()
				.getSupportedLanguages()
				.stream()
				.map(SupportedLanguage::locale)
				.collect(Collectors.toSet());
	}

	protected static Resource newNotification() {
		ResourceBuilder notificationBuilder = new ResourceBuilder(TYPE_NOTIFICATION, TYPE_NOTIFICATION)

				.bag(BAG_VISIBILITY, TYPE_VISIBILITY)

				.date(PARAM_VISIBLE_FROM).end()

				.date(PARAM_VISIBLE_TO).end()

				.booleanB(PARAM_FOR_ALL).end()

				.stringList(PARAM_ROLES).end()

				.stringList(PARAM_LOCATIONS).end()

				.endBag();

		return notificationBuilder.build();
	}
}
