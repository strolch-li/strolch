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

	public static final String PATH_EMPTY_RUNTIME = "target/QueryTest/"; //$NON-NLS-1$

	@Test
	public void shouldQueryResourceWithParamValue() {

		StrolchAgent agent = ComponentContainerTest.startContainer(PATH_EMPTY_RUNTIME,
				ComponentContainerTest.PATH_EMPTY_CONTAINER);
		Resource res1 = createResource("@1", "Test Resource", "MyType");
		IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
		res1.addParameter(BAG_ID, iP);
		agent.getResourceMap().add(res1);

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

		StrolchAgent agent = ComponentContainerTest.startContainer(PATH_EMPTY_RUNTIME,
				ComponentContainerTest.PATH_EMPTY_CONTAINER);
		Order o1 = createOrder("@1", "Test Order", "MyType");
		IntegerParameter iP = new IntegerParameter("nbOfBooks", "Number of Books", 33);
		o1.addParameter(BAG_ID, iP);
		agent.getOrderMap().add(o1);

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
