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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddOrderCollectionCommandTest extends AbstractRealmCommandTest {

	private List<Order> orders;

	@Before
	public void before() {
		this.orders = new ArrayList<>();
		this.orders.add(ModelGenerator.createOrder("firstOrder", "First Order", "AdditionalOrders"));
		this.orders.add(ModelGenerator.createOrder("secondOrder", "Second Order", "AdditionalOrders"));
		this.orders.add(ModelGenerator.createOrder("thirdOrder", "Third Order", "AdditionalOrders"));
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		AddOrderCollectionCommand command = new AddOrderCollectionCommand(tx);
		command.setOrders(this.orders);
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		for (Order order : orders) {
			assertTrue(tx.getOrderMap().hasElement(tx, order.getType(), order.getId()));
		}
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		for (Order order : orders) {
			assertFalse(tx.getOrderMap().hasElement(tx, order.getType(), order.getId()));
		}
	}
}
