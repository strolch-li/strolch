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

import static li.strolch.model.query.ParameterSelection.booleanSelection;
import static li.strolch.model.query.ParameterSelection.floatListSelection;
import static li.strolch.model.query.ParameterSelection.floatSelection;
import static li.strolch.model.query.ParameterSelection.integerListSelection;
import static li.strolch.model.query.ParameterSelection.longListSelection;
import static li.strolch.model.query.ParameterSelection.stringListSelection;
import static li.strolch.model.query.ParameterSelection.stringSelection;
import static li.strolch.utils.StringMatchMode.ci;
import static li.strolch.utils.StringMatchMode.es;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.State;
import li.strolch.model.Version;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.FloatListParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerListParameter;
import li.strolch.model.parameter.LongListParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.query.IdSelection;
import li.strolch.model.query.NameSelection;
import li.strolch.model.query.OrderQuery;
import li.strolch.model.query.OrderStateSelection;
import li.strolch.model.query.ParameterSelection;
import li.strolch.persistence.inmemory.InMemoryOrderDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class InMemoryOrderQueryTest {

	protected InMemoryOrderDao daoInstance() {
		return new InMemoryOrderDao(false);
	}

	@Test
	public void shouldQueryById() {

		List<Order> orders = getOrders();
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> orderQuery = OrderQuery.query("MyType1");
		orderQuery.with(new IdSelection("@1"));

		List<Order> result = dao.doQuery(orderQuery);
		assertEquals(1, result.size());
		assertEquals("@1", result.get(0).getId());
	}

	@Test
	public void shouldQueryByIdOr() {

		List<Order> orders = getOrders();
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> orderQuery = OrderQuery.query("MyType2");
		orderQuery.or().with(new IdSelection("@3"), new IdSelection("@4"));

		List<Order> result = dao.doQuery(orderQuery);
		assertEquals(2, result.size());
		assertEquals("@3", result.get(0).getId());
		assertEquals("@4", result.get(1).getId());
	}

	@Test
	public void shouldQueryByIdAnd() {

		List<Order> orders = getOrders();
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> orderQuery = OrderQuery.query("MyType2");
		orderQuery.and().with(new IdSelection("@3"), new NameSelection("Order 3", es()));

		List<Order> result = dao.doQuery(orderQuery);
		assertEquals(1, result.size());
		assertEquals("@3", result.get(0).getId());
	}

	@Test
	public void shouldNotQueryByIdAnd() {

		List<Order> orders = getOrders();
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> orderQuery = OrderQuery.query("MyType1");
		orderQuery.and().with(new IdSelection("@3"), new NameSelection("@4", es()));

		List<Order> result = dao.doQuery(orderQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByParameter() {

		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("Ball");
		ballQuery.and().with(
				//
				stringSelection("parameters", "color", "red", es()),
				booleanSelection("parameters", "forChildren", true), floatSelection("parameters", "diameter", 22.0));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByListParameter() {

		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery;
		List<Order> result;

		// string list
		{
			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a", "z")));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(stringListSelection("parameters", "stringListValues", Arrays.asList("c", "b", "a")));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// integer list
		{
			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1, 5)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(integerListSelection("parameters", "intListValues", Arrays.asList(3, 2, 1)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// float list
		{
			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0, 8.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(floatListSelection("parameters", "floatListValues", Arrays.asList(6.2, 5.1, 4.0)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}

		// long list
		{
			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L, 11L)));
			result = dao.doQuery(ballQuery);
			assertEquals(0, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());

			ballQuery = OrderQuery.query("Ball");
			ballQuery.and().with(longListSelection("parameters", "longListValues", Arrays.asList(10L, 9L, 8L)));
			result = dao.doQuery(ballQuery);
			assertEquals(1, result.size());
		}
	}

	@Test
	public void shouldQueryByNullParameter1() {
		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "color"));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(0, result.size());
	}

	@Test
	public void shouldQueryByNullParameter2() {
		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByNullParameter3() {
		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("Ball");
		ballQuery.and().with( //
				ParameterSelection.nullSelection("parameters", "weight"));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByName() {

		List<Order> orders = getOrders();
		orders.add(getBallOrder());
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("Ball");
		ballQuery.with(new NameSelection("ball ", ci()));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	@Test
	public void shouldQueryByState() {

		List<Order> orders = getOrders();
		InMemoryOrderDao dao = daoInstance();
		dao.saveAll(orders);

		OrderQuery<Order> ballQuery = OrderQuery.query("MyType1");
		ballQuery.with(new OrderStateSelection(State.STOPPED));

		List<Order> result = dao.doQuery(ballQuery);
		assertEquals(2, result.size());

		ballQuery = OrderQuery.query("MyType2");
		ballQuery.with(new OrderStateSelection(State.STOPPED));
		result = dao.doQuery(ballQuery);
		assertEquals(1, result.size());
	}

	private Order getBallOrder() {
		Order o1 = new Order("childrensBall", "Ball 1", "Ball");
		Version.setInitialVersionFor(o1, "test");
		ParameterBag bag = new ParameterBag("parameters", "Ball Details", "Parameters");
		bag.addParameter(new StringParameter("color", "Color", "red"));
		bag.addParameter(new BooleanParameter("forChildren", "Color", true));
		bag.addParameter(new FloatParameter("diameter", "Color", 22.0));
		bag.addParameter(
				new StringListParameter("stringListValues", "List of String Values", Arrays.asList("a", "b", "c")));
		bag.addParameter(new IntegerListParameter("intListValues", "List of Integer Values", Arrays.asList(1, 2, 3)));
		bag.addParameter(
				new FloatListParameter("floatListValues", "List of Float Values", Arrays.asList(4.0, 5.1, 6.2)));
		bag.addParameter(new LongListParameter("longListValues", "List of Long Values", Arrays.asList(8L, 9L, 10L)));
		o1.addParameterBag(bag);
		return o1;
	}

	private List<Order> getOrders() {
		Order res1 = ModelGenerator.createOrder("@1", "Order 1", "MyType1", new Date(), State.STOPPED);
		Order res2 = ModelGenerator.createOrder("@2", "Order 2", "MyType1", new Date(), State.STOPPED);
		Order res3 = ModelGenerator.createOrder("@3", "Order 3", "MyType2", new Date(), State.STOPPED);
		Order res4 = ModelGenerator.createOrder("@4", "Order 4", "MyType2", new Date(), State.PLANNING);
		Order res5 = ModelGenerator.createOrder("@5", "Order 5", "MyType3", new Date(), State.ERROR);
		Order res6 = ModelGenerator.createOrder("@6", "Order 6", "MyType3", new Date(), State.CLOSED);
		List<Order> orders = new ArrayList<>();
		orders.add(res1);
		orders.add(res2);
		orders.add(res3);
		orders.add(res4);
		orders.add(res5);
		orders.add(res6);

		for (Order order : orders) {
			Version.setInitialVersionFor(order, "test");
		}
		return orders;
	}
}
