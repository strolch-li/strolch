package li.strolch.testbase.runtime;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.ArchiveTransaction;
import li.strolch.persistence.api.DataArchiveHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class DataArchiveTestRunner {

	private final Certificate certificate;
	private final StrolchRealm realm;
	private final DataArchiveHandler archiveHandler;

	public DataArchiveTestRunner(RuntimeMock runtimeMock, String realmName) {
		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
		this.realm = runtimeMock.getRealm(realmName);
		this.archiveHandler = runtimeMock.getComponent(DataArchiveHandler.class);
	}

	public void runTestsForDataArchive() throws Exception {
		runResourceTests();
		runOrderTests();
		runActivityTests();
	}

	private void runResourceTests() throws Exception {

		// create a resource
		Resource resource1 = createResource("myTestResource1", "Test Name", "QTestType1");

		// check it doesn't exist
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Resource archivedRes = archiveTx.getResourceBy(resource1.getType(), resource1.getId());
				assertNull("Expected resource to not exist in archive!", archivedRes);
			}

			tx.commitOnClose();
		}

		// add to archive
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				archiveTx.addResource(resource1);
			}

			tx.commitOnClose();
		}

		// check it exists now
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Resource archivedRes = archiveTx.getResourceBy(resource1.getType(), resource1.getId());
				assertNotNull("Expected resource to exist in archive after saving!", archivedRes);
			}

			tx.commitOnClose();
		}
	}

	private void runOrderTests() throws Exception {

		// create an order
		Order order1 = createOrder("myTestOrder1", "Test Name", "QTestType1");

		// check it doesn't exist
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Order archivedOrder = archiveTx.getOrderBy(order1.getType(), order1.getId());
				assertNull("Expected order to not exist in archive!", archivedOrder);
			}

			tx.commitOnClose();
		}

		// add to archive
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				archiveTx.addOrder(order1);
			}

			tx.commitOnClose();
		}

		// check it exists now
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Order archivedOrder = archiveTx.getOrderBy(order1.getType(), order1.getId());
				assertNotNull("Expected order to exist in archive after saving!", archivedOrder);
			}

			tx.commitOnClose();
		}
	}

	private void runActivityTests() throws Exception {

		// create an activity
		Activity activity1 = createActivity("myTestActivity1", "Test Name", "QTestType1", TimeOrdering.SERIES);

		// check it doesn't exist
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Activity archivedActivity = archiveTx.getActivityBy(activity1.getType(), activity1.getId());
				assertNull("Expected activity to not exist in archive!", archivedActivity);
			}

			tx.commitOnClose();
		}

		// add to archive
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				archiveTx.addActivity(activity1);
			}

			tx.commitOnClose();
		}

		// check it exists now
		try (StrolchTransaction tx = this.realm.openTx(this.certificate, "test", false)) {
			try (ArchiveTransaction archiveTx = this.archiveHandler.openArchiveTx(tx)) {
				Activity archivedActivity = archiveTx.getActivityBy(activity1.getType(), activity1.getId());
				assertNotNull("Expected activity to exist in archive after saving!", archivedActivity);
			}

			tx.commitOnClose();
		}
	}
}
