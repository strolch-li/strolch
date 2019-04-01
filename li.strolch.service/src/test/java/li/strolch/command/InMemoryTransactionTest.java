package li.strolch.command;

import static li.strolch.service.test.AbstractRealmServiceTest.*;
import static org.junit.Assert.*;

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
import org.junit.BeforeClass;
import org.junit.Test;

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

	protected StrolchTransaction openTx(String realmName, boolean readOnly) {
		return runtimeMock.getAgent().getContainer().getRealm(realmName).openTx(certificate, "test", readOnly);
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
		try (StrolchTransaction tx = openTx(realmName, true)) {

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
		try (StrolchTransaction tx = openTx(realmName, true)) {

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
		try (StrolchTransaction tx = openTx(realmName, true)) {

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
		try (StrolchTransaction tx = openTx(realmName, false)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newRes);

			tx.add(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Resource should exist!", tx.hasResource(type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Resource res = tx.getResourceBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, res);
			tx.assertHasPrivilege(Operation.UPDATE, res);

			res.setName("Foo foo");
			tx.update(res);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName, true)) {
			Resource res = tx.getResourceBy(type, id);
			assertEquals("Foo foo", res.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Resource res = tx.getResourceBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, res);

			tx.remove(res);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Resource should not exist!", tx.hasResource(type, id));
		}

		// create again
		newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Resource should exist!", tx.hasResource(type, id));
		}
	}

	public void shouldCrudOrder(String realmName) {
		String id = "@200";
		String type = "Bike";

		// create
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newOrder);

			tx.add(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Order should exist!", tx.hasOrder(type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Order order = tx.getOrderBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, order);
			tx.assertHasPrivilege(Operation.UPDATE, order);

			order.setName("Foo foo");
			tx.update(order);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName, true)) {
			Order order = tx.getOrderBy(type, id);
			assertEquals("Foo foo", order.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Order order = tx.getOrderBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, order);

			tx.remove(order);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Order should not exist!", tx.hasOrder(type, id));
		}

		// create again
		newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Order should exist!", tx.hasOrder(type, id));
		}
	}

	public void shouldCrudActivity(String realmName) {
		String id = "@200";
		String type = "Bike";

		// create
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName, false)) {

			// privilege assertion
			tx.assertHasPrivilege(Operation.ADD, newActivity);

			tx.add(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Activity should exist!", tx.hasActivity(type, id));
		}

		// update
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Activity activity = tx.getActivityBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.GET, activity);
			tx.assertHasPrivilege(Operation.UPDATE, activity);

			activity.setName("Foo foo");
			tx.update(activity);
			tx.commitOnClose();
		}

		// verify
		try (StrolchTransaction tx = openTx(realmName, true)) {
			Activity activity = tx.getActivityBy(type, id);
			assertEquals("Foo foo", activity.getName());
		}

		// remove
		try (StrolchTransaction tx = openTx(realmName, false)) {
			Activity activity = tx.getActivityBy(type, id);

			// privilege assertion
			tx.assertHasPrivilege(Operation.REMOVE, activity);

			tx.remove(activity);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Activity should not exist!", tx.hasActivity(type, id));
		}

		// create again
		newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Activity should exist!", tx.hasActivity(type, id));
		}
	}

	public void shouldCrudResource1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newRes);
			newRes.setName("Foo foo!");
			tx.update(newRes);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Resource should exist!", tx.hasResource(type, id));
		}
	}

	public void shouldCrudResource2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create, update and remove
		Resource newRes = ModelGenerator.createResource(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newRes);
			newRes.setName("Foo foo!");
			tx.update(newRes);
			tx.remove(newRes);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Resource should not exist!", tx.hasResource(type, id));
		}
	}

	public void shouldCrudOrder1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newOrder);
			newOrder.setName("Foo foo!");
			tx.update(newOrder);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Order should exist!", tx.hasOrder(type, id));
		}
	}

	public void shouldCrudOrder2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create and update
		Order newOrder = ModelGenerator.createOrder(id, "200", type);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newOrder);
			newOrder.setName("Foo foo!");
			tx.update(newOrder);
			tx.remove(newOrder);
			tx.commitOnClose();
		}

		// create, update and remove
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Order should not exist!", tx.hasOrder(type, id));
		}
	}

	public void shouldCrudActivity1(String realmName) {
		String id = "@201";
		String type = "Bike";

		// create and update
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newActivity);
			newActivity.setName("Foo foo!");
			tx.update(newActivity);
			tx.commitOnClose();
		}

		// should exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertTrue("Activity should exist!", tx.hasActivity(type, id));
		}
	}

	public void shouldCrudActivity2(String realmName) {
		String id = "@202";
		String type = "Bike";

		// create, update and remove
		Activity newActivity = ModelGenerator.createActivity(id, "200", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = openTx(realmName, false)) {
			tx.add(newActivity);
			newActivity.setName("Foo foo!");
			tx.update(newActivity);
			tx.remove(newActivity);
			tx.commitOnClose();
		}

		// should not exist
		try (StrolchTransaction tx = openTx(realmName, true)) {
			assertFalse("Activity should not exist!", tx.hasActivity(type, id));
		}
	}
}
