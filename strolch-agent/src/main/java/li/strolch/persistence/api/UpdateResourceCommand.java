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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class UpdateResourceCommand extends Command {

	private Resource resource;
	private Resource replaced;
	private boolean updated;

	/**
	 * @param tx
	 */
	public UpdateResourceCommand(StrolchTransaction tx) {
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
		this.replaced = resourceMap.getBy(tx(), this.resource.getType(), this.resource.getId());
		if (this.replaced == null) {
			String msg = "The Resource {0} can not be updated as it does not exist!!";
			msg = MessageFormat.format(msg, this.resource.getLocator());
			throw new StrolchException(msg);
		}

		resourceMap.update(tx(), this.resource);
		this.updated = true;
	}

	@Override
	public void undo() {
		if (this.updated && tx().isRollingBack()) {
			if (tx().isVersioningEnabled())
				tx().getResourceMap().undoVersion(tx(), this.resource);
			else
				tx().getResourceMap().update(tx(), this.replaced);
		}
	}
}
