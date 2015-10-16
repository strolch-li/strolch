package ch.eitchnet.privilege.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class PersistSessionsTest extends AbstractPrivilegeTest {

	@BeforeClass
	public static void init() throws Exception {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
		prepareConfigs(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml", "PrivilegeModel.xml");
	}

	@AfterClass
	public static void destroy() throws Exception {
		removeConfigs(PersistSessionsTest.class.getSimpleName());
	}

	@Before
	public void setup() throws Exception {
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
	}

	@Test
	public void shouldPersistAndReloadSessions() {

		// assert no sessions file
		File sessionsFile = new File("target/PersistSessionsTest/sessions.dat");
		assertFalse("Sessions File should no yet exist", sessionsFile.exists());

		// login and assert sessions file was written
		login("admin", "admin".getBytes());
		this.privilegeHandler.isCertificateValid(ctx.getCertificate());
		assertTrue("Sessions File should have been created!", sessionsFile.isFile());

		// re-initialize and assert still logged in
		initialize(PersistSessionsTest.class.getSimpleName(), "PrivilegeConfig.xml");
		this.privilegeHandler.isCertificateValid(ctx.getCertificate());
	}
}
