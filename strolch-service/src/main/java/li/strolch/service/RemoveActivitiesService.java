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

import li.strolch.model.Locator;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveActivitiesService extends AbstractService<LocatorListArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public LocatorListArgument getArgumentInstance() {
		return new LocatorListArgument();
	}

	@Override
	protected ServiceResult internalDoService(LocatorListArgument arg) {

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			for (Locator locator : arg.locators) {
				Activity activity = tx.findElement(locator);
				tx.remove(activity);
			}
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
