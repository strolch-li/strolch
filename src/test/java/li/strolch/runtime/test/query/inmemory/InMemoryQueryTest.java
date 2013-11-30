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

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterizedElement;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.runtime.query.inmemory.AndSelector;
import li.strolch.runtime.query.inmemory.BooleanSelector;
import li.strolch.runtime.query.inmemory.IdSelector;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.ListNavigator;
import li.strolch.runtime.query.inmemory.NameSelector;
import li.strolch.runtime.query.inmemory.OrSelector;
import li.strolch.runtime.query.inmemory.ParameterSelector;
import li.strolch.runtime.query.inmemory.ParameterizedElementSelector;
import li.strolch.runtime.query.inmemory.Selector;
import li.strolch.runtime.test.query.ModelBuilder;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class InMemoryQueryTest {

	@Test
	public void shouldQueryOrderById() {

		List<Order> orders = getOrders();

		InMemoryQuery<Order> orderQuery = new InMemoryQuery<>();
		orderQuery.setNavigator(new ListNavigator<>(orders));
		orderQuery.addSelector(new IdSelector<Order>("@1"));

		List<Order> result = orderQuery.doQuery();
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryResourceById() {

		List<Resource> resources = getResources();

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new ListNavigator<>(resources));
		resourceQuery.addSelector(new IdSelector<Resource>("@1"));

		List<Resource> result = resourceQuery.doQuery();
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryResourceByIdOr() {

		List<Resource> resources = getResources();

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new ListNavigator<>(resources));
		BooleanSelector<Resource> andSelector = new OrSelector<>(new IdSelector<Resource>("@3"),
				new IdSelector<Resource>("@4"));
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery();
		assertEquals(2, result.size());
		assertEquals("@3", result.get(0).getId());
		assertEquals("@4", result.get(1).getId());
	}

	@Test
	public void shouldQueryResourceByIdAnd() {

		List<Resource> resources = getResources();

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new ListNavigator<>(resources));
		List<Selector<Resource>> andSelectors = new ArrayList<>();
		andSelectors.add(new IdSelector<Resource>("@3"));
		andSelectors.add(new NameSelector<Resource>("Res 3"));
		BooleanSelector<Resource> andSelector = new AndSelector<Resource>(andSelectors);
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery();
		assertEquals(1, result.size());
		assertEquals("@3", result.get(0).getId());
	}

	@Test
	public void shouldNotQueryResourceByIdAnd() {

		List<Resource> resources = getResources();

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new ListNavigator<>(resources));
		List<Selector<Resource>> andSelectors = new ArrayList<>();
		andSelectors.add(new IdSelector<Resource>("@3"));
		andSelectors.add(new NameSelector<Resource>("Res 4"));
		BooleanSelector<Resource> andSelector = new AndSelector<Resource>(andSelectors);
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery();
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByParameter() {

		List<Resource> resources = getResources();
		resources.add(getBallResource());

		InMemoryQuery<Resource> ballQuery = new InMemoryQuery<>();
		ballQuery.setNavigator(new ListNavigator<>(resources));
		List<Selector<ParameterizedElement>> ballDetailSelectors = new ArrayList<>();
		ballDetailSelectors.add(ParameterSelector.stringSelector("color", "red"));
		ballDetailSelectors.add(ParameterSelector.booleanSelector("forChildren", true));
		ballDetailSelectors.add(ParameterSelector.floatSelector("diameter", 22.0));
		ballQuery.addSelector(new ParameterizedElementSelector<Resource>("parameters", ballDetailSelectors));
		
		List<Resource> result = ballQuery.doQuery();
		assertEquals(1, result.size());
	}

	private Resource getBallResource() {
		Resource res1 = new Resource("childrensBall", "Ball 1", "Ball");
		ParameterBag bag = new ParameterBag("parameters", "Ball Details", "Parameters");
		bag.addParameter(new StringParameter("color", "Color", "red"));
		bag.addParameter(new BooleanParameter("forChildren", "Color", true));
		bag.addParameter(new FloatParameter("diameter", "Color", 22.0));
		res1.addParameterBag(bag);
		return res1;
	}

	private List<Resource> getResources() {
		Resource res1 = ModelBuilder.createResource("@1", "Res 1", "MyType1");
		Resource res2 = ModelBuilder.createResource("@2", "Res 2", "MyType1");
		Resource res3 = ModelBuilder.createResource("@3", "Res 3", "MyType2");
		Resource res4 = ModelBuilder.createResource("@4", "Res 4", "MyType2");
		Resource res5 = ModelBuilder.createResource("@5", "Res 5", "MyType3");
		Resource res6 = ModelBuilder.createResource("@6", "Res 6", "MyType3");
		List<Resource> resources = new ArrayList<>();
		resources.add(res1);
		resources.add(res2);
		resources.add(res3);
		resources.add(res4);
		resources.add(res5);
		resources.add(res6);
		return resources;
	}

	private List<Order> getOrders() {
		Order res1 = ModelBuilder.createOrder("@1", "Res 1", "MyType1", new Date(), State.CREATED);
		Order res2 = ModelBuilder.createOrder("@2", "Res 2", "MyType1", new Date(), State.CREATED);
		Order res3 = ModelBuilder.createOrder("@3", "Res 3", "MyType2", new Date(), State.CREATED);
		Order res4 = ModelBuilder.createOrder("@4", "Res 4", "MyType2", new Date(), State.CREATED);
		Order res5 = ModelBuilder.createOrder("@5", "Res 5", "MyType3", new Date(), State.CREATED);
		Order res6 = ModelBuilder.createOrder("@6", "Res 6", "MyType3", new Date(), State.CREATED);
		List<Order> orders = new ArrayList<>();
		orders.add(res1);
		orders.add(res2);
		orders.add(res3);
		orders.add(res4);
		orders.add(res5);
		orders.add(res6);
		return orders;
	}
}
