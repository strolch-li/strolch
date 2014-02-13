package li.strolch.testbase.runtime;

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

import li.strolch.agent.api.OrderMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

@SuppressWarnings("nls")
public class OrderModelTestRunner {

	private static final String ID = "@testOrder";
	private static final String NAME = "Test Order";
	private static final String TYPE = "ToStock";

	private RuntimeMock runtimeMock;

	public OrderModelTestRunner(RuntimeMock runtimeMock) {
		this.runtimeMock = runtimeMock;
	}

	public void runCreateOrderTest() {

		// create
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().add(tx, newOrder);
		}
	}

	public void runQuerySizeTest() {
		
		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().removeAll(tx, tx.getOrderMap().getAllElements(tx));
		}

		// create three orders
		Order order1 = createOrder("myTestOrder1", "Test Name", "QTestType1"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Order order2 = createOrder("myTestOrder2", "Test Name", "QTestType2"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Order order3 = createOrder("myTestOrder3", "Test Name", "QTestType3"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().add(tx, order1);
			tx.getOrderMap().add(tx, order2);
			tx.getOrderMap().add(tx, order3);
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
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
	}

	public void runCrudTests() {

		// create
		Order newOrder = createOrder(ID, NAME, TYPE);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().add(tx, newOrder);
		}

		// read
		Order readOrder = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			readOrder = tx.getOrderMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, readOrder);

		// update
		Parameter<String> sParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!";
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().update(tx, readOrder);
		}

		// read updated
		Order updatedOrder = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			updatedOrder = tx.getOrderMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, updatedOrder);
		if (this.runtimeMock.getContainer().getDataStoreMode() != DataStoreMode.CACHED)
			assertFalse("Objects can't be the same reference after re-reading!", readOrder == updatedOrder);
		Parameter<String> updatedParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			tx.getOrderMap().remove(tx, readOrder);
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx();) {
			Order order = tx.getOrderMap().getBy(tx, TYPE, ID);
			assertNull("Should no read Order with id " + ID, order);
		}
	}

	public void runBulkOperationTests() {

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

		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			OrderMap orderMap = tx.getOrderMap();
			orderMap.removeAll(tx, orderMap.getAllElements(tx));
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			tx.getOrderMap().addAll(tx, orders);
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			List<Order> allOrders = tx.getOrderMap().getAllElements(tx);
			Collections.sort(allOrders, comparator);
			assertEquals(orders, allOrders);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
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

		try (StrolchTransaction tx = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			Order order = tx.getOrderMap().getBy(tx, "MyType1", "@_00000001");
			assertNotNull(order);
			order = tx.getOrderMap().getBy(tx, "MyType2", "@_00000006");
			assertNotNull(order);
			order = tx.getOrderMap().getBy(tx, "MyType3", "@_00000011");
			assertNotNull(order);
		}
	}
}
