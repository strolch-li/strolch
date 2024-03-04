package li.strolch.service.notifications;

import com.google.gson.JsonObject;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.builder.ResourceBuilder;
import li.strolch.persistence.api.Operation;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.SimpleRestrictable;
import li.strolch.runtime.configuration.SupportedLanguage;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.collections.Tuple;
import li.strolch.utils.dbc.DBC;

import java.util.Set;
import java.util.stream.Collectors;

import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.privilege.handler.DefaultPrivilegeHandler.PRIVILEGE_GET_ROLE;
import static li.strolch.utils.iso8601.ISO8601.parseToZdt;

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

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			Resource notification = buildNotification(tx, jsonObject, getSupportedLanguages(getAgent()));
			tx.add(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	protected static Resource buildNotification(StrolchTransaction tx, JsonObject jsonObject,
			Set<String> supportedLanguages) {
		Resource notification = newNotification();
		PrivilegeContext ctx = tx.getPrivilegeContext();

		JsonObject visibilityJ = jsonObject.get(BAG_VISIBILITY).getAsJsonObject();
		ParameterBag visibility = notification.getParameterBag(BAG_VISIBILITY);

		visibility.setBoolean(PARAM_FOR_ALL,
				visibilityJ.has(PARAM_FOR_ALL) && visibilityJ.get(PARAM_FOR_ALL).getAsBoolean());
		if (visibilityJ.has(PARAM_VISIBLE_FROM))
			visibility.setDate(PARAM_VISIBLE_FROM, parseToZdt(visibilityJ.get(PARAM_VISIBLE_FROM).getAsString()));
		if (visibilityJ.has(PARAM_VISIBLE_TO))
			visibility.setDate(PARAM_VISIBLE_TO, parseToZdt(visibilityJ.get(PARAM_VISIBLE_TO).getAsString()));

		if (visibilityJ.has(PARAM_ROLES)) {
			String rolesJ = visibilityJ.get(PARAM_ROLES).getAsString();
			visibility.getStringListP(PARAM_ROLES).setValueFromString(rolesJ);
			for (String role : visibility.getStringList(PARAM_ROLES)) {
				ctx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_ROLE, new Tuple(null, role)));
			}
		}

		if (visibilityJ.has(PARAM_LOCATIONS)) {
			String locationsJ = visibilityJ.get(PARAM_LOCATIONS).getAsString();
			visibility.getStringListP(PARAM_LOCATIONS).setValueFromString(locationsJ);
			for (String locationId : visibility.getStringList(PARAM_LOCATIONS)) {
				tx.assertHasPrivilege(Operation.GET, tx.getResourceBy(TYPE_LOCATION, locationId, true));
			}
		}

		for (String language : supportedLanguages) {
			if (!jsonObject.has(language))
				continue;
			JsonObject languageJ = jsonObject.get(language).getAsJsonObject();
			String title = languageJ.get(PARAM_TITLE).getAsString();
			String text = languageJ.get(PARAM_TEXT).getAsString();

			ParameterBag languageBag = new ParameterBag(language, language, TYPE_TEXT);
			languageBag.setString(PARAM_TITLE, title);
			languageBag.setString(PARAM_TEXT, text);
			notification.addParameterBag(languageBag);
		}
		return notification;
	}

	public static Set<String> getSupportedLanguages(StrolchAgent agent) {
		return agent
				.getStrolchConfiguration()
				.getRuntimeConfiguration()
				.getSupportedLanguages()
				.stream()
				.map(SupportedLanguage::locale)
				.collect(Collectors.toSet());
	}

	public static Resource newNotification() {
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
