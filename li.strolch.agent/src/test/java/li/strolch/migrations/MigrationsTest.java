package li.strolch.migrations;

import static li.strolch.agent.ComponentContainerTest.destroyContainer;
import static li.strolch.agent.ComponentContainerTest.logger;
import static li.strolch.agent.ComponentContainerTest.startContainer;

import java.util.Map;
import java.util.Map.Entry;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.runtime.privilege.PrivilegeHandler;

import org.junit.Test;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;
import ch.eitchnet.utils.collections.MapOfLists;

public class MigrationsTest {

	@Test
	public void shouldRunMigrations() {

		try {
			StrolchAgent agent = startContainer("target/MigrationsTest/", "src/test/resources/migrationstest");

			PrivilegeHandler privilegeHandler = agent.getContainer().getPrivilegeHandler();
			Certificate cert = privilegeHandler.authenticate("test", "test".getBytes());

			MigrationsHandler migrationsHandler = agent.getContainer().getComponent(MigrationsHandler.class);
			Map<String, Version> currentVersions = migrationsHandler.getCurrentVersions(cert);
			for (Entry<String, Version> entry : currentVersions.entrySet()) {
				logger.info("[" + entry.getKey() + "] Current version: " + entry.getValue());
			}

			MapOfLists<String, Version> migrationsToRun = migrationsHandler.queryMigrationsToRun(cert);
			for (String realm : migrationsToRun.keySet()) {
				logger.info("[" + realm + "] Migrations to run: " + migrationsToRun.getList(realm));
			}

			destroyContainer(agent);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw e;
		}
	}
}
