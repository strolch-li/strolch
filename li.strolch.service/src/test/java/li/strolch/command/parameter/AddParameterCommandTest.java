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
package li.strolch.command.parameter;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.command.AbstractRealmCommandTest;
import li.strolch.model.Locator;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddParameterCommandTest extends AbstractRealmCommandTest {

	private Locator locator;
	private Parameter<?> parameter;

	@Before
	public void before() {
		this.locator = Locator.valueOf("Resource/Ball/yellow/Bag/parameters");
		this.parameter = new BooleanParameter("newParam", "New Param", false);
	}

	@Override
	protected Command getCommandInstance(ComponentContainer container, StrolchTransaction tx) {

		ParameterizedElement element = tx.findElement(this.locator);

		AddParameterCommand command = new AddParameterCommand(container, tx);
		command.setElement(element);
		command.setParameter(this.parameter);
		return command;
	}

	@Override
	protected void validateAfterCommand(ComponentContainer container, StrolchTransaction tx) {
		ParameterizedElement element = tx.findElement(this.locator);
		assertTrue(element.hasParameter(this.parameter.getId()));
	}

	@Override
	protected void validateAfterCommandFailed(ComponentContainer container, StrolchTransaction tx) {
		ParameterizedElement element = tx.findElement(this.locator);
		assertFalse(element.hasParameter(this.parameter.getId()));
	}
}
