/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.agent.impl;

import java.util.List;

import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditingResourceMap extends AuditingElementMapFacade<Resource> implements ResourceMap {

	/**
	 * @param elementMap
	 */
	public AuditingResourceMap(ElementMap<Resource> elementMap, boolean observeAccessReads) {
		super(elementMap, observeAccessReads);
	}

	@Override
	protected ResourceMap getElementMap() {
		return (ResourceMap) super.getElementMap();
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery query, ResourceVisitor<U> resourceVisitor) {
		List<U> result = getElementMap().doQuery(tx, query, resourceVisitor);
		// TODO decide how to audit these queried elements
		return result;
	}
}
