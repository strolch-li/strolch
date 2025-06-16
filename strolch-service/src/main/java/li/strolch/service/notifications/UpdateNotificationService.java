/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			Resource notification = buildNotification(tx, jsonObject, getSupportedLanguages(getAgent()));
			notification.setId(arg.objectId);

			tx.update(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
