/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service;

import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.UpdateActivityCommand;
import li.strolch.service.UpdateActivityService.UpdateActivityArg;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateActivityService extends AbstractService<UpdateActivityArg, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public UpdateActivityArg getArgumentInstance() {
		return new UpdateActivityArg();
	}

	@Override
	protected ServiceResult internalDoService(UpdateActivityArg arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			if (arg.refreshUnknownVersion && !arg.activity.hasVersion()) {
				Activity current = tx.getActivityBy(arg.activity.getType(), arg.activity.getId(), true);
				arg.activity.setVersion(current.getVersion());
			}

			UpdateActivityCommand command = new UpdateActivityCommand(getContainer(), tx);
			command.setActivity(arg.activity);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class UpdateActivityArg extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public boolean refreshUnknownVersion = false;
		public Activity activity;
	}
}
