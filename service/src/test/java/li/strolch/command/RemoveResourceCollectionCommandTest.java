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

import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveResourceCollectionCommandTest extends AbstractRealmCommandTest {

	private List<Locator> locators;

	@Before
	public void before() {
		this.locators = new ArrayList<>();
		this.locators.add(Locator.newBuilder(Tags.RESOURCE, "Enumeration", "salutations").build());
		this.locators.add(Locator.newBuilder(Tags.RESOURCE, "Enumeration", "sex").build());
		this.locators.add(Locator.newBuilder(Tags.RESOURCE, "Enumeration", "religions").build());
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		List<Resource> resources = new ArrayList<>(this.locators.size());
		for (Locator locator : this.locators) {
			resources.add((Resource) tx.findElement(locator));
		}

		RemoveResourceCollectionCommand command = new RemoveResourceCollectionCommand(tx);
		command.setResources(resources);
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		for (Locator locator : locators) {
			assertFalse(tx.getResourceMap().hasElement(tx, locator.get(1), locator.get(2)));
		}
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		for (Locator locator : locators) {
			assertTrue(tx.getResourceMap().hasElement(tx, locator.get(1), locator.get(2)));
		}
	}
}
