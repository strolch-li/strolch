package li.strolch.testbase.runtime;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;

import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;

public class VersioningTestRunner {

	private RuntimeMock runtimeMock;
	private Certificate certificate;

	public VersioningTestRunner(RuntimeMock runtimeMock) {
		this.runtimeMock = runtimeMock;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runTestsForVersioning() {
		assumeTrue(runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).isVersioningEnabled());

		runResourceTests();
		runOrderTests();
		runActivityTests();
	}

	private void runResourceTests() {
		ComponentContainer container = runtimeMock.getContainer();

		// initialize by adding a resource
		String type = "TestType";
		String id = StrolchAgent.getUniqueId();
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = ModelGenerator.createResource(id, "Test Name", type);
			tx.add(res1);
			tx.flush();
			// must be first version
			assertEquals(0, res1.getVersion().getVersion());
			tx.commitOnClose();
		}

		// first make sure that the we can't change anything without updating the model
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			// must be first version
			assertEquals(0, res1.getVersion().getVersion());
			res1.setName("Something");

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			assertEquals("Test Name", res1.getName());

			tx.commitOnClose();
		}

		// now do a change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			res1.setName("Something");
			tx.update(res1);

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			assertEquals("Something", res1.getName());
			// version must be incremented
			assertEquals(1, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// now revert the change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource revertedVersion = tx.getResourceMap().revertToVersion(tx, type, id, 0);
			assertEquals("Test Name", revertedVersion.getName());
			// version must be incremented
			assertEquals(2, revertedVersion.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			assertEquals("Test Name", res1.getName());
			// version must be incremented
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo a version in same TX
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);

			// create a new version:
			res1.setName("Version 3");
			tx.update(res1);
			tx.flush();

			// version must be incremented
			assertEquals(3, res1.getVersion().getVersion());

			// now undo
			tx.getResourceMap().undoVersion(tx, res1);
			tx.clearCache();

			// and validate we have again version 2
			res1 = tx.getResourceBy(type, id, true);
			assertEquals("Test Name", res1.getName());
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy(type, id, true);
			assertEquals("Test Name", res1.getName());
			// version must be incremented
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo all versions
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1;

			// undo three times as we have version 0, 1, 2
			res1 = tx.getResourceBy(type, id, true);
			assertEquals(2, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);
			tx.clearCache();

			res1 = tx.getResourceBy(type, id, true);
			assertEquals(1, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);
			tx.clearCache();

			res1 = tx.getResourceBy(type, id, true);
			assertEquals(0, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);
			tx.clearCache();

			// and validate all are deleted
			assertFalse(tx.hasResource(type, id));

			tx.commitOnClose();
		}

		// do a deletion
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = ModelGenerator.createResource("ball", "Red Ball", "Ball");
			assertNull(res1.getVersion());

			tx.add(res1);
			tx.flush();
			assertEquals(0, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy("Ball", "ball", true);
			assertEquals("Red Ball", res1.getName());

			tx.remove(res1);
			tx.flush();
			// version must be incremented
			assertEquals(1, res1.getVersion().getVersion());

			res1 = tx.getResourceBy("Ball", "ball");
			assertNull(res1);

			assertFalse(tx.hasResource("Ball", "ball"));

			tx.commitOnClose();
		}

		// restore a version manually
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy("Ball", "ball");
			assertNull(res1);

			List<Resource> versions = tx.getResourceMap().getVersionsFor(tx, "Ball", "ball");
			assertEquals(2, versions.size());

			res1 = versions.get(versions.size() - 1);
			assertTrue(res1.getVersion().isDeleted());
			tx.add(res1);
			tx.flush();
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res1 = tx.getResourceBy("Ball", "ball");
			assertNotNull(res1);
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// do a create, get, remove, and re-create of the elements
		Resource res1 = ModelGenerator.createResource(StrolchAgent.getUniqueId(), "Test Name", type);
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(res1);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasResource(res1.getType(), res1.getId()));
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Resource res = tx.getResourceBy(res1.getType(), res1.getId());
			assertNotNull(res);
			tx.remove(res);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(res1.getClone());
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasResource(res1.getType(), res1.getId()));
		}
	}

	private void runOrderTests() {
		ComponentContainer container = runtimeMock.getContainer();

		// initialize by adding a order
		String type = "TestType";
		String id = StrolchAgent.getUniqueId();
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = ModelGenerator.createOrder(id, "Test Name", type);
			tx.add(order1);
			tx.flush();
			// must be first version
			assertEquals(0, order1.getVersion().getVersion());
			tx.commitOnClose();
		}

		// first make sure that the we can't change anything without updating the model
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			// must be first version
			assertEquals(0, order1.getVersion().getVersion());
			order1.setName("Something");

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			assertEquals("Test Name", order1.getName());

			tx.commitOnClose();
		}

		// now do a change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			order1.setName("Something");
			tx.update(order1);

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			assertEquals("Something", order1.getName());
			// version must be incremented
			assertEquals(1, order1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// now revert the change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order revertedVersion = tx.getOrderMap().revertToVersion(tx, type, id, 0);
			assertEquals("Test Name", revertedVersion.getName());
			// version must be incremented
			assertEquals(2, revertedVersion.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			assertEquals("Test Name", order1.getName());
			// version must be incremented
			assertEquals(2, order1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo a version in same TX
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);

			// create a new version:
			order1.setName("Version 3");
			tx.update(order1);
			tx.flush();

			// version must be incremented
			assertEquals(3, order1.getVersion().getVersion());

			// now undo
			tx.getOrderMap().undoVersion(tx, order1);
			tx.clearCache();

			// and validate we have again version 2
			order1 = tx.getOrderBy(type, id, true);
			assertEquals("Test Name", order1.getName());
			assertEquals(2, order1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy(type, id, true);
			assertEquals("Test Name", order1.getName());
			// version must be incremented
			assertEquals(2, order1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo all versions
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1;

			// undo three times as we have version 0, 1, 2
			order1 = tx.getOrderBy(type, id, true);
			assertEquals(2, order1.getVersion().getVersion());
			tx.getOrderMap().undoVersion(tx, order1);
			tx.clearCache();

			order1 = tx.getOrderBy(type, id, true);
			assertEquals(1, order1.getVersion().getVersion());
			tx.getOrderMap().undoVersion(tx, order1);
			tx.clearCache();

			order1 = tx.getOrderBy(type, id, true);
			assertEquals(0, order1.getVersion().getVersion());
			tx.getOrderMap().undoVersion(tx, order1);
			tx.clearCache();

			// and validate all are deleted
			assertFalse(tx.hasOrder(type, id));

			tx.commitOnClose();
		}

		// do a deletion
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = ModelGenerator.createOrder("ball", "Red Ball", "Ball");
			assertNull(order1.getVersion());

			tx.add(order1);
			tx.flush();
			assertEquals(0, order1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy("Ball", "ball", true);
			assertEquals("Red Ball", order1.getName());

			tx.remove(order1);
			tx.flush();
			// version must be incremented
			assertEquals(1, order1.getVersion().getVersion());

			order1 = tx.getOrderBy("Ball", "ball");
			assertNull(order1);

			assertFalse(tx.hasOrder("Ball", "ball"));

			tx.commitOnClose();
		}

		// restore a version manually
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy("Ball", "ball");
			assertNull(order1);

			List<Order> versions = tx.getOrderMap().getVersionsFor(tx, "Ball", "ball");
			assertEquals(2, versions.size());

			order1 = versions.get(versions.size() - 1);
			assertTrue(order1.getVersion().isDeleted());
			tx.add(order1);
			tx.flush();
			assertEquals(2, order1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order order1 = tx.getOrderBy("Ball", "ball");
			assertNotNull(order1);
			assertEquals(2, order1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// do a create, get, remove, and re-create of the elements
		Order order1 = ModelGenerator.createOrder(StrolchAgent.getUniqueId(), "Test Name", type);
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(order1);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasOrder(order1.getType(), order1.getId()));
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Order res = tx.getOrderBy(order1.getType(), order1.getId());
			assertNotNull(res);
			tx.remove(res);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(order1.getClone());
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasOrder(order1.getType(), order1.getId()));
		}
	}

	private void runActivityTests() {
		ComponentContainer container = runtimeMock.getContainer();

		// initialize by adding a activity
		String type = "TestType";
		String id = StrolchAgent.getUniqueId();
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = ModelGenerator.createActivity(id, "Test Name", type, TimeOrdering.SERIES);
			tx.add(act1);
			tx.flush();
			// must be first version
			assertEquals(0, act1.getVersion().getVersion());
			tx.commitOnClose();
		}

		// first make sure that the we can't change anything without updating the model
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			// must be first version
			assertEquals(0, act1.getVersion().getVersion());
			act1.setName("Something");

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			assertEquals("Test Name", act1.getName());

			tx.commitOnClose();
		}

		// now do a change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			act1.setName("Something");
			tx.update(act1);

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			assertEquals("Something", act1.getName());
			// version must be incremented
			assertEquals(1, act1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// now revert the change
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity revertedVersion = tx.getActivityMap().revertToVersion(tx, type, id, 0);
			assertEquals("Test Name", revertedVersion.getName());
			// version must be incremented
			assertEquals(2, revertedVersion.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			assertEquals("Test Name", act1.getName());
			// version must be incremented
			assertEquals(2, act1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo a version in same TX
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);

			// create a new version:
			act1.setName("Version 3");
			tx.update(act1);
			tx.flush();

			// version must be incremented
			assertEquals(3, act1.getVersion().getVersion());

			// now undo
			tx.getActivityMap().undoVersion(tx, act1);
			tx.clearCache();

			// and validate we have again version 2
			act1 = tx.getActivityBy(type, id, true);
			assertEquals("Test Name", act1.getName());
			assertEquals(2, act1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy(type, id, true);
			assertEquals("Test Name", act1.getName());
			// version must be incremented
			assertEquals(2, act1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo all versions
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1;

			// undo three times as we have version 0, 1, 2
			act1 = tx.getActivityBy(type, id, true);
			assertEquals(2, act1.getVersion().getVersion());
			tx.getActivityMap().undoVersion(tx, act1);
			tx.clearCache();

			act1 = tx.getActivityBy(type, id, true);
			assertEquals(1, act1.getVersion().getVersion());
			tx.getActivityMap().undoVersion(tx, act1);
			tx.clearCache();

			act1 = tx.getActivityBy(type, id, true);
			assertEquals(0, act1.getVersion().getVersion());
			tx.getActivityMap().undoVersion(tx, act1);
			tx.clearCache();

			// and validate all are deleted
			assertFalse(tx.hasActivity(type, id));

			tx.commitOnClose();
		}

		// do a deletion
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = ModelGenerator.createActivity("ball", "Red Ball", "Ball", TimeOrdering.SERIES);
			assertNull(act1.getVersion());

			tx.add(act1);
			tx.flush();
			assertEquals(0, act1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy("Ball", "ball", true);
			assertEquals("Red Ball", act1.getName());

			tx.remove(act1);
			tx.flush();

			// version must be incremented
			assertEquals(1, act1.getVersion().getVersion());

			act1 = tx.getActivityBy("Ball", "ball");
			assertNull(act1);

			assertFalse(tx.hasActivity("Ball", "ball"));

			tx.commitOnClose();
		}

		// restore a version manually
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy("Ball", "ball");
			assertNull(act1);

			List<Activity> versions = tx.getActivityMap().getVersionsFor(tx, "Ball", "ball");
			assertEquals(2, versions.size());

			act1 = versions.get(versions.size() - 1);
			assertTrue(act1.getVersion().isDeleted());
			tx.add(act1);
			tx.flush();
			assertEquals(2, act1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity act1 = tx.getActivityBy("Ball", "ball");
			assertNotNull(act1);
			assertEquals(2, act1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// do a create, get, remove, and re-create of the elements
		Activity act1 = ModelGenerator
				.createActivity(StrolchAgent.getUniqueId(), "Test Name", type, TimeOrdering.SERIES);
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(act1);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasActivity(act1.getType(), act1.getId()));
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			Activity res = tx.getActivityBy(act1.getType(), act1.getId());
			assertNotNull(res);
			tx.remove(res);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			tx.add(act1.getClone());
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate)
				.openTx(certificate, VersioningTestRunner.class, false)) {
			assertTrue(tx.hasActivity(act1.getType(), act1.getId()));
		}
	}

}
