/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.StringArgument;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

import static li.strolch.model.StrolchModelConstants.TYPE_NOTIFICATION;

public class RemoveNotificationService extends AbstractService<StringArgument, ServiceResult> {
	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public StringArgument getArgumentInstance() {
		return new StringArgument();
	}

	@Override
	protected ServiceResult internalDoService(StringArgument arg) throws Exception {
		DBC.PRE.assertNotEmpty("value must be set", arg.value);

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			Resource notification = tx.getResourceBy(TYPE_NOTIFICATION, arg.value, true);
			tx.remove(notification);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
