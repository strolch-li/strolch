/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.service.test;

import static li.strolch.testbase.runtime.RuntimeMock.assertServiceResult;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.service.XmlImportModelArgument;
import li.strolch.service.XmlImportModelService;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.api.ServiceHandler;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.api.ServiceResultState;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.After;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.db.DbSchemaVersionCheck;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.Version;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRealmServiceTest {

	public static final String REALM_CACHED = "svcCached";
	public static final String REALM_TRANSACTIONAL = "svcTransactional";
	public static final String REALM_TRANSIENT = "svcTransient";
	public static final String RUNTIME_PATH = "target/svcTestRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/svctest"; //$NON-NLS-1$

	protected static final Logger logger = LoggerFactory.getLogger(AbstractRealmServiceTest.class);

	protected static RuntimeMock runtimeMock;
	protected Certificate certificate;

	@Before
	public void before() throws Exception {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");
		dropSchema("jdbc:postgresql://localhost/transactionaluserdb", "transactionaluser", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		this.certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
		importFromXml(REALM_CACHED, this.certificate, getServiceHandler());
		importFromXml(REALM_TRANSACTIONAL, this.certificate, getServiceHandler());
	}

	@After
	public void after() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	public static void dropSchema(String dbUrl, String dbUsername, String dbPassword) throws Exception {
		Version dbVersion = DbSchemaVersionCheck.getExpectedDbVersion(PostgreSqlPersistenceHandler.SCRIPT_PREFIX,
				PostgreSqlPersistenceHandler.class);
		String sql = DbSchemaVersionCheck.getSql(PostgreSqlPersistenceHandler.SCRIPT_PREFIX,
				PostgreSqlPersistenceHandler.class, dbVersion, "drop"); //$NON-NLS-1$
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	public static void importFromXml(String realm, Certificate certificate, ServiceHandler serviceHandler) {

		XmlImportModelService svc = new XmlImportModelService();
		XmlImportModelArgument arg = new XmlImportModelArgument();
		arg.realm = realm;
		arg.modelFileName = "StrolchModel.xml";
		ServiceResult result = serviceHandler.doService(certificate, svc, arg);
		assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
	}

	protected <T extends ServiceArgument, U extends ServiceResult> void doService(String realm,
			ServiceResultState expectedState, Class<?> expectedServiceResultType, Service<T, U> svc, T arg,
			Runner before, Runner validator, Runner after) {

		if (before != null)
			before.run(runtimeMock.getContainer().getRealm(realm), runtimeMock.getContainer());

		arg.realm = realm;

		ServiceResult result = getServiceHandler().doService(this.certificate, svc, arg);
		assertServiceResult(expectedState, expectedServiceResultType, result);

		if (validator != null)
			validator.run(runtimeMock.getContainer().getRealm(realm), runtimeMock.getContainer());

		if (after != null)
			after.run(runtimeMock.getContainer().getRealm(realm), runtimeMock.getContainer());
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}

	public interface Runner {
		public void run(StrolchRealm strolchRealm, ComponentContainer container);
	}

	protected <T extends ServiceArgument, U extends ServiceResult> void runServiceInAllRealmTypes(
			Class<? extends Service<T, U>> svcClass, T arg) {
		runServiceInAllRealmTypes(svcClass, arg, null, null, null);
	}

	protected <T extends ServiceArgument, U extends ServiceResult> void runServiceInAllRealmTypes(
			Class<? extends Service<T, U>> svcClass, T arg, Runner before, Runner validator, Runner after) {

		try {
			runTransient(svcClass.newInstance(), arg, before, validator, after);
			runCached(svcClass.newInstance(), arg, before, validator, after);
			runTransactional(svcClass.newInstance(), arg, before, validator, after);
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException("Failed to instantiate class " + svcClass.getName() + ": " + e.getMessage(), e);
		}
	}

	private <T extends ServiceArgument, U extends ServiceResult> void runTransactional(Service<T, U> svc, T arg,
			Runner before, Runner validator, Runner after) {
		doService(REALM_TRANSACTIONAL, ServiceResultState.SUCCESS, ServiceResult.class, svc, arg, before, validator,
				after);
	}

	private <T extends ServiceArgument, U extends ServiceResult> void runCached(Service<T, U> svc, T arg,
			Runner before, Runner validator, Runner after) {
		doService(REALM_CACHED, ServiceResultState.SUCCESS, ServiceResult.class, svc, arg, before, validator, after);
	}

	private <T extends ServiceArgument, U extends ServiceResult> void runTransient(Service<T, U> svc, T arg,
			Runner before, Runner validator, Runner after) {
		doService(REALM_TRANSIENT, ServiceResultState.SUCCESS, ServiceResult.class, svc, arg, before, validator, after);
	}
}
