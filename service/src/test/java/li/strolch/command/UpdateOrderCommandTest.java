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
package li.strolch.command;

import static org.junit.Assert.assertEquals;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.UpdateOrderCommand;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateOrderCommandTest extends AbstractRealmCommandTest {

	private Order order;

	@Before
	public void before() {
		this.order = ModelGenerator.createOrder("myCarOrder", "Modified Car Order", "ProductionOrder");
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		UpdateOrderCommand command = new UpdateOrderCommand(tx);
		command.setOrder(this.order.getClone(true));
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		Order o = tx.getOrderBy(order.getType(), order.getId());
		assertEquals("Modified Car Order", o.getName());
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		Order o = tx.getOrderBy(order.getType(), order.getId());
		assertEquals("Car Production Order", o.getName());
	}
}
