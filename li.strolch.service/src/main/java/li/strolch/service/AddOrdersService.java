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
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceResult;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddOrdersService extends AbstractService<StrolchRootElementListArgument, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult();
	}

	@Override
	public StrolchRootElementListArgument getArgumentInstance() {
		return new StrolchRootElementListArgument();
	}

	@Override
	protected ServiceResult internalDoService(StrolchRootElementListArgument arg) {
		DBC.PRE.assertNotNull("root elements must not be null!", arg.rootElements);

		try (StrolchTransaction tx = openArgOrUserTx(arg)) {
			for (StrolchRootElement rootElement : arg.rootElements) {
				DBC.PRE.assertEquals("Expected an order!", Tags.ORDER, rootElement.getObjectType());
				tx.add((Order) rootElement);
			}
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}
}
