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

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.inmemory.InMemoryOrderDao;
import li.strolch.persistence.inmemory.InMemoryResourceDao;
import li.strolch.runtime.query.inmemory.AndSelector;
import li.strolch.runtime.query.inmemory.AnyNavigator;
import li.strolch.runtime.query.inmemory.BooleanSelector;
import li.strolch.runtime.query.inmemory.IdSelector;
import li.strolch.runtime.query.inmemory.InMemoryQuery;
import li.strolch.runtime.query.inmemory.NameSelector;
import li.strolch.runtime.query.inmemory.OrSelector;
import li.strolch.runtime.query.inmemory.ParameterSelector;
import li.strolch.runtime.query.inmemory.Selector;

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
		InMemoryOrderDao dao = new InMemoryOrderDao();
		dao.saveAll(orders);

		InMemoryQuery<Order> orderQuery = new InMemoryQuery<>();
		orderQuery.setNavigator(new AnyNavigator<Order>());
		orderQuery.addSelector(new IdSelector<Order>("@1"));

		List<Order> result = orderQuery.doQuery(dao);
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryResourceById() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = new InMemoryResourceDao();
		dao.saveAll(resources);

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new AnyNavigator<Resource>());
		resourceQuery.addSelector(new IdSelector<Resource>("@1"));

		List<Resource> result = resourceQuery.doQuery(dao);
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryResourceByIdOr() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = new InMemoryResourceDao();
		dao.saveAll(resources);

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new AnyNavigator<Resource>());
		BooleanSelector<Resource> andSelector = new OrSelector<>(new IdSelector<Resource>("@3"),
				new IdSelector<Resource>("@4"));
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery(dao);
		assertEquals(2, result.size());
		assertEquals("@3", result.get(0).getId());
		assertEquals("@4", result.get(1).getId());
	}

	@Test
	public void shouldQueryResourceByIdAnd() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = new InMemoryResourceDao();
		dao.saveAll(resources);

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new AnyNavigator<Resource>());
		List<Selector<Resource>> andSelectors = new ArrayList<>();
		andSelectors.add(new IdSelector<Resource>("@3"));
		andSelectors.add(new NameSelector<Resource>("Res 3"));
		BooleanSelector<Resource> andSelector = new AndSelector<Resource>(andSelectors);
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery(dao);
		assertEquals(1, result.size());
		assertEquals("@3", result.get(0).getId());
	}

	@Test
	public void shouldNotQueryResourceByIdAnd() {

		List<Resource> resources = getResources();
		InMemoryResourceDao dao = new InMemoryResourceDao();
		dao.saveAll(resources);

		InMemoryQuery<Resource> resourceQuery = new InMemoryQuery<>();
		resourceQuery.setNavigator(new AnyNavigator<Resource>());
		List<Selector<Resource>> andSelectors = new ArrayList<>();
		andSelectors.add(new IdSelector<Resource>("@3"));
		andSelectors.add(new NameSelector<Resource>("Res 4"));
		BooleanSelector<Resource> andSelector = new AndSelector<Resource>(andSelectors);
		resourceQuery.addSelector(andSelector);

		List<Resource> result = resourceQuery.doQuery(dao);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByParameter() {

		List<Resource> resources = getResources();
		resources.add(getBallResource());
		InMemoryResourceDao dao = new InMemoryResourceDao();
		dao.saveAll(resources);

		InMemoryQuery<Resource> ballQuery = new InMemoryQuery<>();
		ballQuery.setNavigator(new AnyNavigator<Resource>());
		ballQuery.addSelector(ParameterSelector.<Resource> stringSelector("parameters", "color", "red"));
		ballQuery.addSelector(ParameterSelector.<Resource> booleanSelector("parameters", "forChildren", true));
		ballQuery.addSelector(ParameterSelector.<Resource> floatSelector("parameters", "diameter", 22.0));

		List<Resource> result = ballQuery.doQuery(dao);
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
		Resource res1 = ModelGenerator.createResource("@1", "Res 1", "MyType1");
		Resource res2 = ModelGenerator.createResource("@2", "Res 2", "MyType1");
		Resource res3 = ModelGenerator.createResource("@3", "Res 3", "MyType2");
		Resource res4 = ModelGenerator.createResource("@4", "Res 4", "MyType2");
		Resource res5 = ModelGenerator.createResource("@5", "Res 5", "MyType3");
		Resource res6 = ModelGenerator.createResource("@6", "Res 6", "MyType3");
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
		Order res1 = ModelGenerator.createOrder("@1", "Res 1", "MyType1", new Date(), State.CREATED);
		Order res2 = ModelGenerator.createOrder("@2", "Res 2", "MyType1", new Date(), State.CREATED);
		Order res3 = ModelGenerator.createOrder("@3", "Res 3", "MyType2", new Date(), State.CREATED);
		Order res4 = ModelGenerator.createOrder("@4", "Res 4", "MyType2", new Date(), State.CREATED);
		Order res5 = ModelGenerator.createOrder("@5", "Res 5", "MyType3", new Date(), State.CREATED);
		Order res6 = ModelGenerator.createOrder("@6", "Res 6", "MyType3", new Date(), State.CREATED);
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
