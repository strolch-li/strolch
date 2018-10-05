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

import java.util.ArrayList;
import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateResourceCollectionCommandTest extends AbstractRealmCommandTest {

	private List<Resource> resources;

	@Before
	public void before() {
		this.resources = new ArrayList<>();
		this.resources.add(ModelGenerator.createResource("salutations", "Modified Enumeration", "Enumeration"));
		this.resources.add(ModelGenerator.createResource("sex", "Modified Enumeration", "Enumeration"));
		this.resources.add(ModelGenerator.createResource("religions", "Modified Enumeration", "Enumeration"));
	}

	@Override
	protected Command getCommandInstance(ComponentContainer container, StrolchTransaction tx) {

		UpdateResourceCollectionCommand command = new UpdateResourceCollectionCommand(container, tx);
		command.setResources(this.resources);
		return command;
	}

	@Override
	protected void validateAfterCommand(ComponentContainer container, StrolchTransaction tx) {
		for (Resource resource : this.resources) {
			Resource r = tx.getResourceBy(resource.getType(), resource.getId());
			assertEquals("Modified Enumeration", r.getName());
		}
	}

	@Override
	protected void validateAfterCommandFailed(ComponentContainer container, StrolchTransaction tx) {
		for (Resource resource : this.resources) {
			Resource r = tx.getResourceBy(resource.getType(), resource.getId());
			assertEquals(r.getId(), r.getName().toLowerCase());
		}
	}
}
