package li.strolch.migrations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.collections.MapOfLists;

public class MigrationsTest {

	public static final String RUNTIME_PATH = "target/migrationstest/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/migrationstest"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;
	protected static Certificate certificate;

	@BeforeClass
	public static void beforeClass() throws Exception {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldRunMigrations() {

		PrivilegeHandler privilegeHandler = runtimeMock.getPrivilegeHandler();
		Certificate cert = privilegeHandler.authenticate("test", "test".getBytes());

		MigrationsHandler migrationsHandler = runtimeMock.getContainer().getComponent(MigrationsHandler.class);
		Map<String, Version> currentVersions = migrationsHandler.getCurrentVersions(cert);
		assertEquals("1.1.1", currentVersions.get(StrolchConstants.DEFAULT_REALM).toString());

		MapOfLists<String, Version> lastMigrations = migrationsHandler.getLastMigrations();
		List<Version> expectedMigrations = Arrays.asList(Version.valueOf("0.1.0"), Version.valueOf("0.1.1"),
				Version.valueOf("0.5.2"), Version.valueOf("1.0.0"), Version.valueOf("1.0.5"), Version.valueOf("1.1.1"));
		assertEquals(expectedMigrations, lastMigrations.getList(StrolchConstants.DEFAULT_REALM));

		MapOfLists<String, Version> migrationsToRun = migrationsHandler.queryMigrationsToRun(cert);
		assertTrue("Expected to have all migrations run", migrationsToRun.isEmpty());
	}
}
