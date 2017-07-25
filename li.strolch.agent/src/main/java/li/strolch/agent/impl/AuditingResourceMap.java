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
package li.strolch.agent.impl;

import java.util.List;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * This is the {@link AuditTrail} for {@link Resource Resources}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @see AuditingElementMapFacade
 */
public class AuditingResourceMap extends AuditingElementMapFacade<Resource> implements ResourceMap {

	public AuditingResourceMap(ElementMap<Resource> elementMap, boolean observeAccessReads) {
		super(elementMap, observeAccessReads);
	}

	@Override
	protected String getElementType() {
		return Tags.RESOURCE;
	}

	@Override
	protected ResourceMap getElementMap() {
		return (ResourceMap) super.getElementMap();
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery<U> query) {
		StrolchElementVisitor<U> resourceVisitor = query.getResourceVisitor();
		DBC.PRE.assertNotNull("resourceVisitor on query", resourceVisitor);
		query.setResourceVisitor(resource -> {
			this.read.add(resource);
			return resource.accept(resourceVisitor);
		});

		return getElementMap().doQuery(tx, query);
	}
}
