/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.test.benchmark;

import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.postgresql.DataType;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.Version;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;
import org.postgresql.Driver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;

import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.db.DbConstants.PROP_DB_HOST_OVERRIDE;
import static li.strolch.model.ModelGenerator.*;
import static li.strolch.runtime.configuration.DbConnectionBuilder.overridePostgresqlHost;

public abstract class PerformanceTest {

	protected static final Logger logger = LoggerFactory.getLogger(PerformanceTest.class);

	protected static RuntimeMock runtimeMock;
	private static Certificate certificate;

	protected RuntimeMock runtime() {
		return runtimeMock;
	}

	public static void buildRuntime(String sourcePath, String targetPath, DataType dataType) {
		if (runtimeMock != null)
			return;
		File configSrc = new File(sourcePath);
		File rootPath = new File(targetPath);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		runtimeMock.getAgent().getComponentO(PersistenceHandler.class).ifPresent(ph -> {
			if (ph instanceof PostgreSqlPersistenceHandler persistenceHandler) {
				DBC.INTERIM.assertEquals("Expected data type " + dataType, dataType, persistenceHandler.getDataType());
			}
		});

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public static void dropSchema(String ctx, String dbUrl, String dbUsername, String dbPassword) throws Exception {

		if (System.getProperties().containsKey(PROP_DB_HOST_OVERRIDE))
			dbUrl = overridePostgresqlHost(ctx, dbUrl);

		if (!Driver.isRegistered())
			Driver.register();

		Version dbVersion = DbSchemaVersionCheck.getExpectedDbVersion(
				PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH, PostgreSqlPersistenceHandler.class);
		logger.info("Dropping schema for expected version {}", dbVersion);
		String sql = DbSchemaVersionCheck.getSql(PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH,
				PostgreSqlPersistenceHandler.class, dbVersion, "drop");
		logger.info(StringHelper.NEW_LINE + "{}", sql);
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	public static void tearDown(String targetPath) throws Exception {
		if (runtimeMock != null) {
			runtimeMock.destroyRuntime();
			runtimeMock = null;
		}

		File rootPath = new File(targetPath);
		if (rootPath.exists()) {
			FileHelper.deleteFile(rootPath, false);
		}

		if (Driver.isRegistered())
			Driver.deregister();
	}

	protected void runCreateElements() {
		Resource newResource = createResource(getUniqueId(), "Test Name", "TestType");
		Order newOrder = createOrder(getUniqueId(), "Test Name", "TestType");
		Activity newActivity = createActivity(getUniqueId(), "Test Name", "TestType", TimeOrdering.SERIES);

		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.add(newResource);
			tx.add(newOrder);
			tx.add(newActivity);
			tx.commitOnClose();
		}
	}

	protected void runCreateResource() {
		Resource newResource = createResource(getUniqueId(), "Test Name", "TestType");
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.add(newResource);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.remove(newResource);
			tx.commitOnClose();
		}
	}

	protected void runCreateOrder() {
		Order newOrder = createOrder(getUniqueId(), "Test Name", "TestType");
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.add(newOrder);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.remove(newOrder);
			tx.commitOnClose();
		}
	}

	protected void runCreateActivity() {
		Activity newActivity = createActivity(getUniqueId(), "Test Name", "TestType", TimeOrdering.SERIES);
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.add(newActivity);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = runtimeMock.openUserTx(certificate, false)) {
			tx.remove(newActivity);
			tx.commitOnClose();
		}
	}
}
