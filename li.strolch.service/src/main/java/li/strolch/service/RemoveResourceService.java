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

import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveResourceService extends AbstractService<LocatorArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	public LocatorArgument getArgumentInstance() {
		return new LocatorArgument();
	}

	@Override
	protected ServiceResult internalDoService(LocatorArgument arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			Resource resource = tx.findElement(arg.locator);
			tx.remove(resource);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
