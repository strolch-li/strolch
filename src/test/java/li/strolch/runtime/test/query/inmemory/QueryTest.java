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
package li.strolch.runtime.test.query.inmemory;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.query.AndSelection;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.ParameterSelection;
import li.strolch.model.query.ResourceQuery;
import li.strolch.model.query.Selection;
import li.strolch.model.query.StrolchTypeNavigation;
import li.strolch.runtime.agent.OrderMap;
import li.strolch.runtime.agent.ResourceMap;
import li.strolch.runtime.agent.StrolchAgent;
import li.strolch.runtime.query.inmemory.InMemoryOrderQueryVisitor;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.InMemoryResourceQueryVisitor;
import li.strolch.runtime.test.component.ComponentContainerTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class QueryTest {

	@Test
	public void shouldQueryResourceWithParamValue() {

		StrolchAgent agent = ComponentContainerTest.startContainer(ComponentContainerTest.PATH_EMPTY_CONTAINER);
		Resource res1 = createResource("@1", "Test Resource", "MyType");
		IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
		res1.addParameter(BAG_ID, iP);
		agent.getContainer().getComponent(ResourceMap.class).add(res1);

		List<Selection> elementAndSelections = new ArrayList<>();
		elementAndSelections.add(new IdSelection("@1"));
		elementAndSelections.add(ParameterSelection.integerSelection(BAG_ID, "nbOfBooks", 33));
		AndSelection<Selection> andSelection = new AndSelection<>(elementAndSelections);
		ResourceQuery query = new ResourceQuery(new StrolchTypeNavigation("MyType"));
		query.addSelection(andSelection);

		InMemoryResourceQueryVisitor resourceQuery = new InMemoryResourceQueryVisitor(agent.getContainer());
		InMemoryQuery<Resource> inMemoryQuery = resourceQuery.visit(query);
		List<Resource> result = inMemoryQuery.doQuery();
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryOrderWithParamValue() {

		StrolchAgent agent = ComponentContainerTest.startContainer(ComponentContainerTest.PATH_EMPTY_CONTAINER);
		Order o1 = createOrder("@1", "Test Order", "MyType");
		IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
		o1.addParameter(BAG_ID, iP);
		agent.getContainer().getComponent(OrderMap.class).add(o1);

		List<Selection> elementAndSelections = new ArrayList<>();
		elementAndSelections.add(new IdSelection("@1"));
		elementAndSelections.add(ParameterSelection.integerSelection(BAG_ID, "nbOfBooks", 33));
		AndSelection<Selection> andSelection = new AndSelection<>(elementAndSelections);
		OrderQuery query = new OrderQuery(new StrolchTypeNavigation("MyType"));
		query.addSelection(andSelection);

		InMemoryOrderQueryVisitor orderQuery = new InMemoryOrderQueryVisitor(agent.getContainer());
		InMemoryQuery<Order> inMemoryQuery = orderQuery.visit(query);
		List<Order> result = inMemoryQuery.doQuery();
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}
}
