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

import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.AddResourceCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddResourceCommandTest extends AbstractRealmCommandTest {

	private Resource resource;

	@Before
	public void before() {
		this.resource = ModelGenerator.createResource("firstRes", "First Resource", "AdditionalResources");
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		AddResourceCommand command = new AddResourceCommand(tx);
		command.setResource(this.resource);
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		assertTrue(tx.getResourceMap().hasElement(tx, this.resource.getType(), this.resource.getId()));
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		assertFalse(tx.getResourceMap().hasElement(tx, this.resource.getType(), this.resource.getId()));
	}
}
