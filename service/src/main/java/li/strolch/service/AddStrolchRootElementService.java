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

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddStrolchRootElementService extends AbstractService<StrolchRootElementArgument, ServiceResult> {

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public StrolchRootElementArgument getArgumentInstance() {
		return new StrolchRootElementArgument();
	}

	@Override
	protected ServiceResult internalDoService(StrolchRootElementArgument arg) {
		DBC.PRE.assertNotNull("root element must not be null!", arg.rootElement);

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {

			switch (arg.rootElement.getObjectType()) {
			case Tags.RESOURCE:
				tx.add((Resource) arg.rootElement);
				break;
			case Tags.ORDER:
				tx.add((Order) arg.rootElement);
				break;
			case Tags.ACTIVITY:
				tx.add((Activity) arg.rootElement);
				break;

			}

			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
