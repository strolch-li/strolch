package li.strolch.service.notifications;

import com.google.gson.JsonObject;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.JsonServiceArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

import static li.strolch.service.notifications.CreateNotificationService.buildNotification;
import static li.strolch.service.notifications.CreateNotificationService.getSupportedLanguages;

public class UpdateNotificationService extends AbstractService<JsonServiceArgument, ServiceResult> {
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
		DBC.PRE.assertNotEmpty("objectId must be set", arg.objectId);
		DBC.PRE.assertNotNull("JsonElement must be set", arg.jsonElement);
		DBC.PRE.assertNotNull("JsonElement must be a JsonObject", arg.jsonElement.isJsonObject());

		JsonObject jsonObject = arg.jsonElement.getAsJsonObject();
		DBC.PRE.assertEquals("arg ID and jsonObject ID must be the same", arg.objectId,
				jsonObject.get(Tags.Json.ID).getAsString());

		Resource notification = buildNotification(jsonObject, getSupportedLanguages(getAgent()));
		notification.setId(arg.objectId);

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			tx.update(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
