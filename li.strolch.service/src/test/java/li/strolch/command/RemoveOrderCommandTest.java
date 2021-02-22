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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.Tags;
import li.strolch.persistence.api.RemoveOrderCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveOrderCommandTest extends AbstractRealmCommandTest {

	private Locator locator;

	@Before
	public void before() throws Exception {
		this.locator = Locator.newBuilder(Tags.ORDER, "TestType", "@3").build();
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		Order order = tx.findElement(this.locator);

		RemoveOrderCommand command = new RemoveOrderCommand(tx);
		command.setOrder(order);
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		assertFalse(tx.getOrderMap().hasElement(tx, "TestType", "@3"));
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		assertTrue(tx.getOrderMap().hasElement(tx, "TestType", "@3"));
	}
}
