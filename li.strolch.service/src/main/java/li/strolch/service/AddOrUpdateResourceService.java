/*
 * Copyright 2017 Reto Breitenmoser <reto.breitenmoser@gmail.com>
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

import li.strolch.command.AddResourceCommand;
import li.strolch.command.UpdateResourceCommand;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Reto Breitenmoser <reto.breitenmoser@gmail.com>
 */
public class AddOrUpdateResourceService extends AbstractService<AddOrUpdateResourceService.AddOrUpdateResourceArg, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	protected ServiceResult internalDoService(AddOrUpdateResourceArg arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			if (tx.getResourceMap().hasElement(tx, arg.resource.getType(), arg.resource.getId())) {
				UpdateResourceCommand updateCmd = new UpdateResourceCommand(getContainer(), tx);
				updateCmd.setResource(arg.resource);
				tx.addCommand(updateCmd);
			} else {
				AddResourceCommand addCmd = new AddResourceCommand(getContainer(), tx);
				addCmd.setResource(arg.resource);
				tx.addCommand(addCmd);
			}
		}

		return ServiceResult.success();
	}

	public static class AddOrUpdateResourceArg extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public Resource resource;
	}
}
