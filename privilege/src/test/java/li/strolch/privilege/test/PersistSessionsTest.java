package li.strolch.privilege.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class PersistSessionsTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
		prepareConfigs(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeUsers.xml",
				"PrivilegeGroups.xml", "PrivilegeRoles.xml");
	}

	@AfterClass
	public static void destroy() {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
	}

	@Before
	public void setup() {
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void shouldPersistAndReloadSessions() throws InterruptedException {

		// assert no sessions file
		File sessionsFile = new File("target/PersistSessionsTest/sessions.dat");
		assertFalse("Sessions File should no yet exist", sessionsFile.exists());

		// login and assert sessions file was written
		login("admin", "admin".toCharArray());
		this.privilegeHandler.validate(ctx.getCertificate());
		// persisting is async, once per second
		Thread.sleep(1200L);
		assertTrue("Sessions File should have been created!", sessionsFile.isFile());

		// re-initialize and assert still logged in
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
		this.privilegeHandler.validate(ctx.getCertificate());
	}
}
