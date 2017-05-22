package li.strolch.testbase.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
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

		ComponentContainer container = runtimeMock.getContainer();

		// initialize by adding a resource
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = ModelGenerator.createResource("MyTestResource", "Test Name", "TestType");
			tx.getResourceMap().add(tx, res1);
			// must be first version
			assertEquals(0, res1.getVersion().getVersion());
			tx.commitOnClose();
		}

		// first make sure that the we can't change anything without updating the model
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			// must be first version
			assertEquals(0, res1.getVersion().getVersion());
			res1.setName("Something");

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals("Test Name", res1.getName());

			tx.commitOnClose();
		}

		// now do a change
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			res1.setName("Something");
			tx.getResourceMap().update(tx, res1);

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals("Something", res1.getName());
			// version must be incremented
			assertEquals(1, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// now revert the change
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource revertedVersion = tx.getResourceMap().revertToVersion(tx, "TestType", "MyTestResource", 0);
			assertEquals("Test Name", revertedVersion.getName());
			// version must be incremented
			assertEquals(2, revertedVersion.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals("Test Name", res1.getName());
			// version must be incremented
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo a version in same TX
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);

			// create a new version:
			res1.setName("Version 3");
			tx.getResourceMap().update(tx, res1);
			// version must be incremented
			assertEquals(3, res1.getVersion().getVersion());

			// now undo
			tx.getResourceMap().undoVersion(tx, res1);

			// and validate we have again version 2
			res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals("Test Name", res1.getName());
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals("Test Name", res1.getName());
			// version must be incremented
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}

		// undo all versions
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1;

			// undo three times as we have version 0, 1, 2
			res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals(2, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);

			res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals(1, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);

			res1 = tx.getResourceBy("TestType", "MyTestResource", true);
			assertEquals(0, res1.getVersion().getVersion());
			tx.getResourceMap().undoVersion(tx, res1);

			// and validate all are deleted
			assertFalse(tx.getResourceMap().hasElement(tx, "TestType", "MyTestResource"));

			tx.commitOnClose();
		}

		// do a deletion
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = ModelGenerator.createResource("ball", "Red Ball", "Ball");
			assertNull(res1.getVersion());

			tx.getResourceMap().add(tx, res1);
			assertEquals(0, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("Ball", "ball", true);
			assertEquals("Red Ball", res1.getName());

			tx.getResourceMap().remove(tx, res1);
			// version must be incremented
			assertEquals(1, res1.getVersion().getVersion());

			res1 = tx.getResourceBy("Ball", "ball");
			assertNull(res1);

			assertFalse(tx.getResourceMap().hasElement(tx, "Ball", "ball"));

			tx.commitOnClose();
		}

		// restore a version manually
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("Ball", "ball");
			assertNull(res1);

			List<Resource> versions = tx.getResourceMap().getVersionsFor(tx, "Ball", "ball");
			assertEquals(2, versions.size());

			res1 = versions.get(versions.size() - 1);
			assertTrue(res1.getVersion().isDeleted());
			tx.getResourceMap().add(tx, res1);
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
		try (StrolchTransaction tx = container.getRealm(certificate).openTx(certificate, VersioningTestRunner.class)) {
			Resource res1 = tx.getResourceBy("Ball", "ball");
			assertNotNull(res1);
			assertEquals(2, res1.getVersion().getVersion());

			tx.commitOnClose();
		}
	}
}
