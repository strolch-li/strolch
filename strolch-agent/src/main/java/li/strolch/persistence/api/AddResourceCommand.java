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
package li.strolch.persistence.api;

import java.text.MessageFormat;

import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddResourceCommand extends Command {

	private Resource resource;
	private boolean added;

	/**
	 * @param tx
	 */
	public AddResourceCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param resource
	 * 		the resource to set
	 */
	public void setResource(Resource resource) {
		this.resource = resource;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Resource may not be null!", this.resource);
	}

	@Override
	public void doCommand() {

		tx().lock(this.resource);

		ResourceMap resourceMap = tx().getResourceMap();
		if (resourceMap.hasElement(tx(), this.resource.getType(), this.resource.getId())) {
			String msg = MessageFormat.format("The Resource {0} already exists!", this.resource.getLocator());
			throw new StrolchException(msg);
		}

		resourceMap.add(tx(), this.resource);
		this.added = true;
	}

	@Override
	public void undo() {
		if (this.added && this.resource != null && tx().isRollingBack()) {
			ResourceMap resourceMap = tx().getResourceMap();
			if (resourceMap.hasElement(tx(), this.resource.getType(), this.resource.getId()))
				resourceMap.remove(tx(), this.resource);
		}
	}
}
