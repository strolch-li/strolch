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
package li.strolch.runtime.query.inmemory;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.Resource;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.ResourceQueryVisitor;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.runtime.component.ComponentContainer;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryResourceQueryVisitor extends InMemoryQueryVisitor<Resource> implements ResourceQueryVisitor {

	private ComponentContainer container;

	private Navigator<Resource> navigator;
	private List<Selector<Resource>> selectors;

	public InMemoryResourceQueryVisitor(ComponentContainer container) {
		super(container);
		this.container = container;
		this.selectors = new ArrayList<>();
	}

	@Override
	protected InMemoryQueryVisitor<Resource> newInstance() {
		return new InMemoryResourceQueryVisitor(this.container);
	}

	public InMemoryQuery<Resource> visit(ResourceQuery resourceQuery) {
		resourceQuery.accept(this);

		if (this.navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		return new InMemoryQuery<>(this.navigator, this.selectors);
	}

	@Override
	public void visit(StrolchTypeNavigation navigation) {
		this.navigator = new ResourceTypeNavigator(navigation.getType(), this.container);
	}
}
