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

import java.util.ArrayList;
import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateOrderCollectionCommandTest extends AbstractRealmCommandTest {

	private List<Order> orders;

	@Before
	public void before() {
		this.orders = new ArrayList<>();
		this.orders.add(ModelGenerator.createOrder("@1", "Modified Test Order", "TestType"));
		this.orders.add(ModelGenerator.createOrder("@2", "Modified Test Order", "TestType"));
		this.orders.add(ModelGenerator.createOrder("@3", "Modified Test Order", "TestType"));
	}

	@Override
	protected Command getCommandInstance(ComponentContainer container, StrolchTransaction tx) {

		UpdateOrderCollectionCommand command = new UpdateOrderCollectionCommand(container, tx);
		command.setOrders(this.orders);
		return command;
	}
}
