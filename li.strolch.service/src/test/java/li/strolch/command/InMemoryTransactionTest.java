package li.strolch.command;

import static li.strolch.service.test.AbstractRealmServiceTest.CONFIG_SRC;
import static li.strolch.service.test.AbstractRealmServiceTest.REALM_CACHED;
import static li.strolch.service.test.AbstractRealmServiceTest.REALM_TRANSIENT;
import static li.strolch.service.test.AbstractRealmServiceTest.dropSchema;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.Operation;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.model.Certificate;
import li.strolch.testbase.runtime.RuntimeMock;

public class InMemoryTransactionTest {

	private static final String TARGET_RUNTIME = "target/InMemoryTransactionTest/";

	private static RuntimeMock runtimeMock;
	private static Certificate certificate;

	@BeforeClass
	public static void beforeClass() throws Exception {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");

		runtimeMock = new RuntimeMock().mockRuntime(TARGET_RUNTIME, CONFIG_SRC);
		runtimeMock.startContainer();
		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	protected StrolchTransaction openTx(String realmName) {
		return runtimeMock.getAgent().getContainer().getRealm(realmName).openTx(certificate, "test");
	}

	@Test
	public void runTransient() {
		runAll(REALM_TRANSIENT);
	}

	@Test
	public void runCached() {
		runAll(REALM_CACHED);
	}

	private void runAll(String realmName) {

		shouldCrudResource(realmName);
		shouldCrudResource1(realmName);
		shouldCrudResource2(realmName);

		shouldCrudOrder(realmName);
		shouldCrudOrder1(realmName);
		shouldCrudOrder2(realmName);

		shouldCrudActivity(realmName);
		shouldCrudActivity1(realmName);
		shouldCrudActivity2(realmName);

		shouldAssertPrivilegeResource(realmName);
		shouldAssertPrivilegeOrder(realmName);
		shouldAssertPrivilegeActivity(realmName);
	}

	private void shouldAssertPrivilegeResource(String realmName) {
		String id = "@203";
		String type = "Car";

		// create
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.GET, newRes);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.ADD, newRes);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.UPDATE, newRes);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.REMOVE, newRes);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}
		}
	}

	private void shouldAssertPrivilegeOrder(String realmName) {
		String id = "@203";
		String type = "Car";

		// create
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.GET, newOrder);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.ADD, newOrder);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.UPDATE, newOrder);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.REMOVE, newOrder);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}
		}
	}

	private void shouldAssertPrivilegeActivity(String realmName) {
		String id = "@203";
		String type = "Car";

		// create
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.GET, newActivity);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.ADD, newActivity);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.UPDATE, newActivity);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}

			// privilege assertion
			try {
				tx.assertHasPrivilege(Operation.REMOVE, newActivity);
				fail();
			} catch (AccessDeniedException e) {
				// as expected
			}
		}
	}

	public void shouldCrudResource(String realmName) {
		String id = "@200";
		String type = "Bike";

		// create
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newRes);

			tx.addResource(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Resource should exist!", tx.getResourceMap().hasElement(tx, type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName)) {
			Resource res = tx.getResourceBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, res);
			tx.assertHasPrivilege(Operation.UPDATE, res);

			res.setName("Foo foo");
			tx.updateResource(res);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName)) {
			Resource res = tx.getResourceBy(type, id);
			assertEquals("Foo foo", res.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName)) {
			Resource res = tx.getResourceBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, res);

			tx.removeResource(res);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Resource should not exist!", tx.getResourceMap().hasElement(tx, type, id));
		}

		// create again
		newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addResource(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Resource should exist!", tx.getResourceMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudOrder(String realmName) {
		String id = "@200";
		String type = "Bike";

		// create
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newOrder);

			tx.addOrder(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Order should exist!", tx.getOrderMap().hasElement(tx, type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName)) {
			Order order = tx.getOrderBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, order);
			tx.assertHasPrivilege(Operation.UPDATE, order);

			order.setName("Foo foo");
			tx.updateOrder(order);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName)) {
			Order order = tx.getOrderBy(type, id);
			assertEquals("Foo foo", order.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName)) {
			Order order = tx.getOrderBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, order);

			tx.removeOrder(order);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Order should not exist!", tx.getOrderMap().hasElement(tx, type, id));
		}

		// create again
		newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addOrder(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Order should exist!", tx.getOrderMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudActivity(String realmName) {
		String id = "@200";
		String type = "Bike";

		// create
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newActivity);

			tx.addActivity(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Activity should exist!", tx.getActivityMap().hasElement(tx, type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName)) {
			Activity activity = tx.getActivityBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, activity);
			tx.assertHasPrivilege(Operation.UPDATE, activity);

			activity.setName("Foo foo");
			tx.updateActivity(activity);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName)) {
			Activity activity = tx.getActivityBy(type, id);
			assertEquals("Foo foo", activity.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName)) {
			Activity activity = tx.getActivityBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, activity);

			tx.removeActivity(activity);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Activity should not exist!", tx.getActivityMap().hasElement(tx, type, id));
		}

		// create again
		newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addActivity(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Activity should exist!", tx.getActivityMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudResource1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addResource(newRes);
			newRes.setName("Foo foo!");
			tx.updateResource(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Resource should exist!", tx.getResourceMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudResource2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create, update and remove
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addResource(newRes);
			newRes.setName("Foo foo!");
			tx.updateResource(newRes);
			tx.removeResource(newRes);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Resource should not exist!", tx.getResourceMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudOrder1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addOrder(newOrder);
			newOrder.setName("Foo foo!");
			tx.updateOrder(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Order should exist!", tx.getOrderMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudOrder2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create and update
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addOrder(newOrder);
			newOrder.setName("Foo foo!");
			tx.updateOrder(newOrder);
			tx.removeOrder(newOrder);
			tx.commitOnClose();
		}

		// create, update and remove
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Order should not exist!", tx.getOrderMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudActivity1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addActivity(newActivity);
			newActivity.setName("Foo foo!");
			tx.updateActivity(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertTrue("Activity should exist!", tx.getActivityMap().hasElement(tx, type, id));
		}
	}

	public void shouldCrudActivity2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create, update and remove
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName)) {
			tx.addActivity(newActivity);
			newActivity.setName("Foo foo!");
			tx.updateActivity(newActivity);
			tx.removeActivity(newActivity);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName)) {
			assertFalse("Activity should not exist!", tx.getActivityMap().hasElement(tx, type, id));
		}
	}
}