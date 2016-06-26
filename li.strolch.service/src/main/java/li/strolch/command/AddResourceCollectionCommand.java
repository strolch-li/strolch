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

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddResourceCollectionCommand extends Command {

	private List<Resource> resources;

	/**
	 * @param tx
	 */
	public AddResourceCollectionCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * @param resources
	 *            the resources to set
	 */
	public void setResources(List<Resource> resources) {
		this.resources = resources;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Resource list may not be null!", this.resources);
	}

	@Override
	public void doCommand() {

		for (Resource resource : this.resources) {
			tx().lock(resource);
		}

		ResourceMap resourceMap = tx().getResourceMap();
		for (Resource resource : this.resources) {
			if (resourceMap.hasElement(tx(), resource.getType(), resource.getId())) {
				String msg = MessageFormat.format("The Resource {0} already exists!", resource.getLocator());
				throw new StrolchException(msg);
			}
		}

		resourceMap.addAll(tx(), this.resources);
	}

	@Override
	public void undo() {
		if (this.resources != null && !this.resources.isEmpty() && tx().isRollingBack()) {
			ResourceMap resourceMap = tx().getResourceMap();
			for (Resource resource : this.resources) {
				if (resourceMap.hasElement(tx(), resource.getType(), resource.getId())) {
					resourceMap.remove(tx(), resource);
				}
			}
		}
	}
}
