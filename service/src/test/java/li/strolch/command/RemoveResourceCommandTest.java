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

import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.persistence.api.RemoveResourceCommand;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveResourceCommandTest extends AbstractRealmCommandTest {

	private Locator locator;

	@Before
	public void before() {
		this.locator = Locator.newBuilder(Tags.RESOURCE, "Enumeration", "sex").build();
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		Resource resource = tx.findElement(this.locator);

		RemoveResourceCommand command = new RemoveResourceCommand(tx);
		command.setResource(resource);
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		assertFalse(tx.getResourceMap().hasElement(tx, "Enumeration", "sex"));
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		assertTrue(tx.getResourceMap().hasElement(tx, "Enumeration", "sex"));
	}
}
