package li.strolch.testbase.runtime;

import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public abstract class AbstractModelTest {

	protected abstract RuntimeMock getRuntimeMock();

	@Test
	public void shouldStartContainer() {
		try (StrolchTransaction tx = getRuntimeMock().getDefaultRealm().openTx()) {
			tx.getOrderDao().queryKeySet();
		}
	}

	@Test
	public void shouldCreateOrders() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock());
		testRunner.runCreateOrderTest();
	}

	@Test
	public void shouldOrderCrud() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock());
		testRunner.runCrudTests();
	}

	@Test
	public void shouldOrderPerformBulkOperations() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock());
		testRunner.runBulkOperationTests();
	}

	@Test
	public void shouldCreateResources() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock());
		testRunner.runCreateResourceTest();
	}

	@Test
	public void shouldResourceCrud() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock());
		testRunner.runCrudTests();
	}

	@Test
	public void shouldResourcePerformBulkOperations() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock());
		testRunner.runBulkOperationTests();
	}
}
