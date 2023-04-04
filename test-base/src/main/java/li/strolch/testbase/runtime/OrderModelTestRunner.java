/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.testbase.runtime;

import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.*;

import java.time.LocalDate;
import java.util.*;

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Order;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.collections.DateRange;

@SuppressWarnings("nls")
public class OrderModelTestRunner {

	private static final String ID = "@testOrder";
	private static final String NAME = "Test Order";
	private static final String TYPE = "ToStock";

	private final RuntimeMock runtimeMock;
	private final String realmName;
	private final Certificate certificate;

	public OrderModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runCreateOrderTest() {

		// create
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}
	}

	public void runQuerySizeTest() {

		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getOrderMap().removeAll(tx, tx.getOrderMap().getAllElements(tx));
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			long size = tx.getOrderMap().querySize(tx);
			assertEquals("Should have 0 objects", 0, size);
		}

		// create three orders
		Order order1 = createOrder("myTestOrder1", "Test Name", "QTestType1"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Order order2 = createOrder("myTestOrder2", "Test Name", "QTestType2"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Order order3 = createOrder("myTestOrder3", "Test Name", "QTestType3"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$

		order1.setDate(LocalDate.of(2018, 3, 1));
		order2.setDate(LocalDate.of(2019, 4, 1));
		order3.setDate(LocalDate.of(2019, 5, 1));

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(order1);
			tx.add(order2);
			tx.add(order3);
			tx.commitOnClose();
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			long size = tx.getOrderMap().querySize(tx);
			assertEquals("Should have three objects", 3, size);

			size = tx.getOrderMap().querySize(tx, "QTestType1");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getOrderMap().querySize(tx, "QTestType2");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getOrderMap().querySize(tx, "QTestType3");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getOrderMap().querySize(tx, "NonExistingType");
			assertEquals("Should have zero objects of type 'NonExistingType'", 0, size);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			PersistenceHandler persistenceHandler = this.runtimeMock.getComponent(PersistenceHandler.class);
			OrderDao dao = persistenceHandler.getOrderDao(tx);

			LocalDate _2017 = LocalDate.of(2017, 1, 1);
			LocalDate _2018 = LocalDate.of(2018, 1, 1);
			LocalDate _20180301 = LocalDate.of(2018, 3, 1);
			LocalDate _20190401 = LocalDate.of(2019, 4, 1);
			LocalDate _20190501 = LocalDate.of(2019, 5, 1);
			LocalDate _2020 = LocalDate.of(2020, 1, 1);

			long size;

			size = dao.querySize(new DateRange().to(_2017, true));
			assertEquals("Expect 0 Orders before 2017 inc", 0, size);

			size = dao.querySize(new DateRange().from(_2017, true).to(_2018, true));
			assertEquals("Expect 0 Orders between _2017 inc and _2018 inc", 0, size);

			size = dao.querySize(new DateRange().from(_2018, true).to(_20180301, false));
			assertEquals("Expect 0 Orders between _2018 inc and _20180301 exc", 0, size);

			size = dao.querySize(new DateRange().from(_2018, true).to(_20180301, true));
			assertEquals("Expect 1 Orders between 2018 inc and _20180301 inc", 1, size);

			size = dao.querySize(new DateRange().from(_20180301, false).to(_20190401, false));
			assertEquals("Expect 0 Orders between _20180301 exc and _20190401 exc", 0, size);

			size = dao.querySize(new DateRange().from(_2017, true));
			assertEquals("Expect 3 Orders from _2017 inc", 3, size);

			size = dao.querySize(new DateRange().from(_2020, true));
			assertEquals("Expect 0 Orders from _2020 inc", 0, size);

			size = dao.querySize(new DateRange().from(_20190501, true).to(_2020, true));
			assertEquals("Expect 1 Orders from _20190501 inc to _2020 inc", 1, size);

			DateRange dateRange = new DateRange().from(_2017, true).to(_20190401, true);
			String[] types = { "QTestType1", "QTestType2", "QTestType3" };

			size = dao.querySize(dateRange, types);
			assertEquals("Expect 2 Orders from _2017 inc to _20190401 inc by types", 2, size);

			List<Order> orders;

			orders = dao.queryAll(dateRange);
			assertEquals("Expect 2 Orders from _2017 inc to _20190401 inc", 2, orders.size());

			if (dao.supportsPaging()) {
				orders = dao.queryAll(dateRange, 2, 1, true);
				assertEquals("Expect 1 Orders from _2017 inc to _20190401 inc offset 1 limit 2", 1, orders.size());
				assertEquals("Expect order myTestOrder2", "myTestOrder2", orders.get(0).getId());

				orders = dao.queryAll(new DateRange().from(_2017, true).to(_2020, true), 2, 1, true);
				assertEquals("Expect 2 Orders from _2017 inc to _2020 inc offset 1 limit 2", 2, orders.size());
				assertEquals("Expect order myTestOrder2", "myTestOrder2", orders.get(0).getId());
				assertEquals("Expect order myTestOrder3", "myTestOrder3", orders.get(1).getId());
			}
		}
	}

	public void runCrudTests() {

		// create
		Order newOrder = createOrder(ID, NAME, TYPE);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}

		// read
		Order readOrder = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			readOrder = tx.getOrderBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, readOrder);

		// update
		StringParameter sParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!";
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.update(readOrder);
			tx.commitOnClose();
		}

		// read updated
		Order updatedOrder = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			updatedOrder = tx.getOrderBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, updatedOrder);
		if (this.runtimeMock.getRealm(this.realmName).getMode() != DataStoreMode.CACHED)
			assertNotSame("Objects can't be the same reference after re-reading!", readOrder, updatedOrder);
		StringParameter updatedParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.remove(readOrder);
			tx.commitOnClose();
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			Order order = tx.getOrderBy(TYPE, ID);
			assertNull("Should not read Order with id " + ID, order);
		}
	}

	public void runBulkOperationTests() {

		// create 15 orders
		List<Order> orders = new ArrayList<>();
		orders.addAll(createOrders(orders.size(), 5, "@", "My Order", "MyType1"));
		orders.addAll(createOrders(orders.size(), 5, "@", "Other Order", "MyType2"));
		orders.addAll(createOrders(orders.size(), 5, "@", "Further Order", "MyType3"));

		// sort them so we know which order our objects are
		orders.sort(Comparator.comparing(StrolchElement::getId));

		// first clear the map, so that we have a clean state
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			OrderMap orderMap = tx.getOrderMap();
			List<Order> allElements = orderMap.getAllElements(tx);
			long removed = orderMap.removeAll(tx);
			assertEquals(allElements.size(), removed);
			assertEquals(0, orderMap.querySize(tx));
			tx.commitOnClose();
		}

		{
			// make sure it is empty
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				OrderMap orderMap = tx.getOrderMap();
				assertEquals(0, orderMap.querySize(tx));
			}

			// now add some orders
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				tx.getOrderMap().addAll(tx, orders);
				tx.commitOnClose();
			}

			// make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				OrderMap orderMap = tx.getOrderMap();
				assertEquals(orders.size(), orderMap.querySize(tx));
				assertEquals(5, orderMap.querySize(tx, "MyType3"));
			}

			// now use the remove all by type
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				tx.getOrderMap().removeAllBy(tx, "MyType3");
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				OrderMap orderMap = tx.getOrderMap();
				assertEquals(orders.size() - 5, orderMap.querySize(tx));
				assertEquals(0, orderMap.querySize(tx, "MyType3"));
			}

			// now use the remove all
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				long removed = tx.getOrderMap().removeAll(tx);
				assertEquals(orders.size() - 5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				OrderMap orderMap = tx.getOrderMap();
				assertEquals(0, orderMap.querySize(tx));
			}
		}

		// remove the version
		orders.forEach(t -> t.setVersion(null));

		// now add all again
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getOrderMap().addAll(tx, orders);
			tx.commitOnClose();
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			List<Order> allOrders = tx.getOrderMap().getAllElements(tx);
			allOrders.sort(Comparator.comparing(StrolchElement::getId));
			assertEquals(orders, allOrders);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			OrderMap orderMap = tx.getOrderMap();

			Set<String> types = orderMap.getTypes(tx);
			assertEquals(expectedTypes, types);

			Set<String> keySet = orderMap.getAllKeys(tx);
			assertEquals(15, keySet.size());

			for (String type : types) {
				Set<String> idsByType = orderMap.getKeysBy(tx, type);
				assertEquals(5, idsByType.size());

				List<Order> ordersByType = orderMap.getElementsBy(tx, type);
				assertEquals(5, ordersByType.size());
			}
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			Order order = tx.getOrderBy("MyType1", "@00000001");
			assertNotNull(order);
			order = tx.getOrderBy("MyType2", "@00000006");
			assertNotNull(order);
			order = tx.getOrderBy("MyType3", "@00000011");
			assertNotNull(order);
		}
	}
}
