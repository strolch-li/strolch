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
package li.strolch.persistence.impl.dao.test;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;
import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createOrders;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public class XmlOrderDaoTest extends AbstractDaoImplTest {

	private static final String ID = "@testOrder"; //$NON-NLS-1$
	private static final String NAME = "Test Order"; //$NON-NLS-1$
	private static final String TYPE = "ToStock"; //$NON-NLS-1$

	@Test
	public void shouldCreateOrder() {

		// create
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getOrderDao().save(newOrder);
		}
	}

	@Test
	public void shouldCrud() {

		// create
		Order newOrder = createOrder(ID, NAME, TYPE);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getOrderDao().save(newOrder);
		}

		// read
		Order readOrder = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			readOrder = tx.getOrderDao().queryBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, readOrder); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getOrderDao().update(readOrder);
		}

		// read updated
		Order updatedOrder = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			updatedOrder = tx.getOrderDao().queryBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, updatedOrder); //$NON-NLS-1$
		assertFalse("Objects can't be the same reference after re-reading!", readOrder == updatedOrder); //$NON-NLS-1$
		Parameter<String> updatedParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getOrderDao().remove(readOrder);
		}

		// fail to re-read
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			Order order = tx.getOrderDao().queryBy(TYPE, ID);
			assertNull("Should no read Order with id " + ID, order); //$NON-NLS-1$
		}
	}

	@SuppressWarnings("nls")
	@Test
	public void shouldPerformBulkOperations() {

		List<Order> orders = new ArrayList<>();
		orders.addAll(createOrders(orders.size(), 5, "@", "My Order ", "MyType1"));
		orders.addAll(createOrders(orders.size(), 5, "@", "Other Order ", "MyType2"));
		orders.addAll(createOrders(orders.size(), 5, "@", "Further Order ", "MyType3"));

		Comparator<Order> comparator = new Comparator<Order>() {
			@Override
			public int compare(Order o1, Order o2) {
				return o1.getId().compareTo(o2.getId());
			}
		};
		Collections.sort(orders, comparator);

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			tx.getOrderDao().removeAll(tx.getOrderDao().queryAll());
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			tx.getOrderDao().saveAll(orders);
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			List<Order> allOrders = tx.getOrderDao().queryAll();
			Collections.sort(allOrders, comparator);
			assertEquals(orders, allOrders);
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			OrderDao orderDao = tx.getOrderDao();

			Set<String> types = orderDao.queryTypes();
			assertEquals(expectedTypes, types);

			Set<String> keySet = orderDao.queryKeySet();
			assertEquals(15, keySet.size());

			for (String type : types) {
				Set<String> idsByType = orderDao.queryKeySet(type);
				assertEquals(5, idsByType.size());

				List<Order> ordersByType = orderDao.queryAll(type);
				assertEquals(5, ordersByType.size());
			}
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			Order order = tx.getOrderDao().queryBy("MyType1", "@_00000001");
			assertNotNull(order);
			order = tx.getOrderDao().queryBy("MyType2", "@_00000006");
			assertNotNull(order);
			order = tx.getOrderDao().queryBy("MyType3", "@_00000011");
			assertNotNull(order);
		}
	}
}
