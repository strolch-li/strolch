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

import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddResourcesCommand extends Command {

	private List<Resource> resources = new ArrayList<>();
	private final List<Resource> added = new ArrayList<>();

	public AddResourcesCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param resources
	 * 		the resources to set for adding
	 */
	public void setResources(List<Resource> resources) {
		this.resources = resources;
	}

	/**
	 * @param resource
	 * 		the resource to add for adding
	 */
	public void addResource(Resource resource) {
		this.resources.add(resource);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Resources may not be null!", this.resources);
	}

	@Override
	public void doCommand() {

		this.resources.forEach(resource -> tx().lock(resource));

		ResourceMap resourceMap = tx().getResourceMap();

		this.resources.forEach(resource -> {
			if (resourceMap.hasElement(tx(), resource.getType(), resource.getId())) {
				String msg = MessageFormat.format("The Resource {0} already exists!", resource.getLocator());
				throw new StrolchException(msg);
			}

			resourceMap.add(tx(), resource);
			this.added.add(resource);
		});
	}

	@Override
	public void undo() {
		if (tx().isRollingBack() && !this.added.isEmpty()) {
			this.added.forEach(resource -> {
				ResourceMap resourceMap = tx().getResourceMap();
				if (resourceMap.hasElement(tx(), resource.getType(), resource.getId()))
					resourceMap.remove(tx(), resource);
			});
		}
	}
}
