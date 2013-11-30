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

import li.strolch.model.StrolchElement;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.query.AndSelection;
import li.strolch.runtime.query.ElementQuery;
import li.strolch.runtime.query.IdSelection;
import li.strolch.runtime.query.NameSelection;
import li.strolch.runtime.query.OrSelection;
import li.strolch.runtime.query.OrderTypeNavigation;
import li.strolch.runtime.query.QueryVisitor;
import li.strolch.runtime.query.ResourceTypeNavigation;
import li.strolch.runtime.query.Selection;
import li.strolch.runtime.query.visitor.StrolchElementVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryQueryVisitor<T extends StrolchElement> implements StrolchElementVisitor {

	private Navigator<T> navigator;
	private List<Selector<T>> selectors;
	private ComponentContainer container;

	public InMemoryQueryVisitor(ComponentContainer container) {
		this.container = container;
		this.selectors = new ArrayList<>();
	}

	/**
	 * @return the navigator
	 */
	public Navigator<T> getNavigator() {
		return this.navigator;
	}

	/**
	 * @return the selectors
	 */
	public List<Selector<T>> getSelectors() {
		return this.selectors;
	}

	public void visit(ElementQuery query) {
		query.visit(this);
	}

	@Override
	public void visitAnd(AndSelection andSelection) {
		InMemoryQueryVisitor<T> visitor = new InMemoryQueryVisitor<>(this.container);
		List<Selection<QueryVisitor>> selections = andSelection.getSelections();
		for (Selection<QueryVisitor> selection : selections) {
			selection.accept(visitor);
		}

		AndSelector<T> andSelector = new AndSelector<>(visitor.getSelectors());
		this.selectors.add(andSelector);
	}

	@Override
	public void visitOr(OrSelection orSelection) {
		InMemoryQueryVisitor<T> visitor = new InMemoryQueryVisitor<>(this.container);
		List<Selection<QueryVisitor>> selections = orSelection.getSelections();
		for (Selection<QueryVisitor> selection : selections) {
			selection.accept(visitor);
		}

		OrSelector<T> andSelector = new OrSelector<>(visitor.getSelectors());
		this.selectors.add(andSelector);
	}

	@Override
	public void visit(IdSelection selection) {
		this.selectors.add(new IdSelector<T>(selection.getId()));
	}

	@Override
	public void visit(NameSelection selection) {
		this.selectors.add(new NameSelector<T>(selection.getName()));
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(OrderTypeNavigation navigation) {
		this.navigator = (Navigator<T>) new OrderTypeNavigator(navigation.getType(), this.container);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(ResourceTypeNavigation navigation) {
		// XXX not good... class should be parameterized, but then there is no sense in ResourceType... but then one would need the ElementMap passed in...
		this.navigator = (Navigator<T>) new ResourceTypeNavigator(navigation.getType(), this.container);
	}
}
