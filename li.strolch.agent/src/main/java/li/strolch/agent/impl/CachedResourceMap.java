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

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_RESOURCE_REF;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.ResourceMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ResourceQuery;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.inmemory.InMemoryResourceDao;

public class CachedResourceMap extends CachedElementMap<Resource> implements ResourceMap {

	private ResourceDao cachedDao;

	public CachedResourceMap() {
		super();
		this.cachedDao = new InMemoryResourceDao();
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {

		String interpretation = refP.getInterpretation();
		if (!interpretation.equals(INTERPRETATION_RESOURCE_REF)) {
			String msg = MessageFormat.format(
					"{0} is not an Resource reference as its interpretation is not {1} it is {2}", //$NON-NLS-1$
					refP.getLocator(), INTERPRETATION_RESOURCE_REF, interpretation);
			throw new StrolchException(msg);
		}

		if (refP.getUom().equals(UOM_NONE)) {
			String msg = MessageFormat.format("{0} is not an Resource reference as its UOM is not set to a type!", //$NON-NLS-1$
					refP.getLocator());
			throw new StrolchException(msg);
		}
	}

	@Override
	protected ResourceDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getResourceDao(tx);
	}

	@Override
	public ResourceDao getCachedDao() {
		return this.cachedDao;
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ResourceQuery query, ResourceVisitor<U> resourceVisitor) {
		return getCachedDao().doQuery(query, resourceVisitor);
	}
}
