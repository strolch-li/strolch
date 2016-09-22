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

import li.strolch.command.UpdateOrderCommand;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateOrderService extends AbstractService<UpdateOrderService.UpdateOrderArg, ServiceResult> {

	private static final long serialVersionUID = 1L;

	@Override
	protected ServiceResult getResultInstance() {
		return new ServiceResult(ServiceResultState.FAILED);
	}

	@Override
	protected ServiceResult internalDoService(UpdateOrderArg arg) {

		try (StrolchTransaction tx = openTx(arg.realm)) {
			UpdateOrderCommand command = new UpdateOrderCommand(getContainer(), tx);
			command.setOrder(arg.order);

			tx.addCommand(command);
			tx.commitOnClose();
		}

		return ServiceResult.success();
	}

	public static class UpdateOrderArg extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public Order order;
	}
}
