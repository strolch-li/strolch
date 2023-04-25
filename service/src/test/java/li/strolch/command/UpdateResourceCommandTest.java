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
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.api.UpdateResourceCommand;
import li.strolch.service.api.Command;
import org.junit.Before;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateResourceCommandTest extends AbstractRealmCommandTest {

	private Resource resource;

	@Before
	public void before() {
		this.resource = ModelGenerator.createResource("yellow", "Modified Yellow Ball", "Ball");
	}

	@Override
	protected Command getCommandInstance(StrolchTransaction tx) {

		UpdateResourceCommand command = new UpdateResourceCommand(tx);
		command.setResource(this.resource.getClone(true));
		return command;
	}

	@Override
	protected void validateAfterCommand(StrolchTransaction tx) {
		Resource r = tx.getResourceBy(resource.getType(), resource.getId());
		assertEquals("Modified Yellow Ball", r.getName());
	}

	@Override
	protected void validateAfterCommandFailed(StrolchTransaction tx) {
		Resource r = tx.getResourceBy(resource.getType(), resource.getId());
		assertEquals("Yellow Ball", r.getName());
	}
}
