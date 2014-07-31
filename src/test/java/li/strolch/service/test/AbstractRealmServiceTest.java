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
import java.sql.SQLException;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.postgresql.DbSchemaVersionCheck;
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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRealmServiceTest {

	public static final String REALM_CACHED = "svcCached";
	public static final String REALM_TRANSACTIONAL = "svcTransactional";
	public static final String REALM_TRANSIENT = "svcTransient";
	public static final String RUNTIME_PATH = "target/svcTestRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/svctest"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@Before
	public void beforeClass() throws SQLException {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");
		dropSchema("jdbc:postgresql://localhost/transactionaluserdb", "transactionaluser", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		importFromXml(REALM_CACHED, getServiceHandler());
		importFromXml(REALM_TRANSACTIONAL, getServiceHandler());
	}

	@After
	public void afterClass() {
		runtimeMock.destroyRuntime();
	}

	public static void dropSchema(String dbUrl, String dbUsername, String dbPassword) throws SQLException {
		String dbVersion = DbSchemaVersionCheck.getExpectedDbVersion();
		String sql = DbSchemaVersionCheck.getSql(dbVersion, "drop"); //$NON-NLS-1$
		try (Connection connection = DriverManager.getConnection(dbUrl, dbUsername, dbPassword)) {
			connection.prepareStatement(sql).execute();
		}
	}

	public static void importFromXml(String realm, ServiceHandler serviceHandler) {

		XmlImportModelService svc = new XmlImportModelService();
		XmlImportModelArgument arg = new XmlImportModelArgument();
		arg.realm = realm;
		arg.modelFileName = "StrolchModel.xml";
		ServiceResult result = serviceHandler.doService(null, svc, arg);
		assertServiceResult(ServiceResultState.SUCCESS, ServiceResult.class, result);
	}

	protected <T extends ServiceArgument, U extends ServiceResult> void doService(String realm,
			ServiceResultState expectedState, Class<?> expectedServiceResultType, Service<T, U> svc, T arg,
			Runner before, Runner validator, Runner after) {

		if (before != null)
			before.run(runtimeMock.getContainer().getRealm(realm), runtimeMock.getContainer());

		arg.realm = realm;
		ServiceResult result = getServiceHandler().doService(null, svc, arg);
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

	protected <T extends ServiceArgument, U extends ServiceResult> void runServiceInAllRealmTypes(Service<T, U> svc,
			T arg) {
		runServiceInAllRealmTypes(svc, arg, null, null, null);
	}

	protected <T extends ServiceArgument, U extends ServiceResult> void runServiceInAllRealmTypes(Service<T, U> svc,
			T arg, Runner before, Runner validator, Runner after) {

		runTransient(svc, arg, before, validator, after);
		runCached(svc, arg, before, validator, after);
		runTransactional(svc, arg, before, validator, after);
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
