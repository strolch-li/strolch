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

import li.strolch.command.UpdateResourceCommand;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.UpdateResourceService.UpdateResourceArg;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateResourceService extends AbstractService<UpdateResourceArg, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public UpdateResourceArg getArgumentInstance() {
		return new UpdateResourceArg();
	}

	@Override
	protected ServiceResult internalDoService(UpdateResourceArg arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			if (arg.refreshUnknownVersion && !arg.resource.hasVersion()) {
				Resource current = tx.getResourceBy(arg.resource.getType(), arg.resource.getId(), true);
				arg.resource.setVersion(current.getVersion());
			}

			UpdateResourceCommand command = new UpdateResourceCommand(getContainer(), tx);
			command.setResource(arg.resource);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class UpdateResourceArg extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public boolean refreshUnknownVersion = false;
		public Resource resource;
	}
}
