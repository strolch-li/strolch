package li.strolch.testbase.runtime;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

import org.junit.Test;

public abstract class AbstractModelTest {

	protected abstract RuntimeMock getRuntimeMock();

	protected String realmName = StrolchConstants.DEFAULT_REALM;

	@Test
	public void shouldStartContainer() {
		try (StrolchTransaction tx = getRuntimeMock().getRealm(this.realmName).openTx()) {
			tx.getOrderMap().getAllKeys(tx);
		}
	}

	@Test
	public void shouldCreateOrders() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCreateOrderTest();
	}

	@Test
	public void shouldQueryOrderSizes() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runQuerySizeTest();
	}

	@Test
	public void shouldOrderCrud() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCrudTests();
	}

	@Test
	public void shouldOrderPerformBulkOperations() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runBulkOperationTests();
	}

	@Test
	public void shouldCreateResources() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCreateResourceTest();
	}

	@Test
	public void shouldQueryResourceSizes() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runQuerySizeTest();
	}

	@Test
	public void shouldResourceCrud() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCrudTests();
	}

	@Test
	public void shouldResourcePerformBulkOperations() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runBulkOperationTests();
	}
}
