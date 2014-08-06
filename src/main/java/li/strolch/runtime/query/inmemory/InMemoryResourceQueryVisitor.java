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
package li.strolch.runtime.query.inmemory;

import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.ResourceVisitor;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.ResourceQueryVisitor;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.persistence.api.ResourceDao;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryResourceQueryVisitor extends InMemoryQueryVisitor<Resource, ResourceDao> implements
		ResourceQueryVisitor {

	public InMemoryResourceQueryVisitor() {
		super();
	}

	@Override
	protected InMemoryQueryVisitor<Resource, ResourceDao> newInstance() {
		return new InMemoryResourceQueryVisitor();
	}

	public <U> InMemoryQuery<Resource, U> visit(ResourceQuery resourceQuery, ResourceVisitor<U> resourceVisitor) {
		DBC.PRE.assertNotNull("ResourceVisitor may not be null!", resourceVisitor);
		resourceQuery.accept(this);

		Navigator<Resource> navigator = getNavigator();
		if (navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		List<Selector<Resource>> selectors = getSelectors();
		if (selectors.isEmpty())
			return new InMemoryQuery<>(navigator, null, resourceVisitor);

		DBC.INTERIM.assertTrue("Invalid query as it may only contain one selector!", selectors.size() == 1); //$NON-NLS-1$
		return new InMemoryQuery<>(navigator, selectors.get(0), resourceVisitor);
	}

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		setNavigator(new ResourceTypeNavigator(navigation.getType()));
	}
}
