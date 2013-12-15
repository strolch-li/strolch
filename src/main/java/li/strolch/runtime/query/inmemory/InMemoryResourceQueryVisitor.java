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
