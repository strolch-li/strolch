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
import li.strolch.db.DbSchemaVersionCheck;
import li.strolch.persistence.postgresql.PostgreSqlPersistenceHandler;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.XmlImportModelArgument;
import li.strolch.service.XmlImportModelResult;
import li.strolch.service.XmlImportModelService;
import li.strolch.service.api.*;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.Version;
import org.junit.After;
import org.junit.Before;
import org.postgresql.Driver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRealmServiceTest<T extends ServiceArgument, U extends ServiceResult> {

	public static final String REALM_CACHED = "svcCached";
	public static final String REALM_CACHED_AUDITS_VERSIONING = "svcCachedAuditsVersioning";
	public static final String REALM_TRANSIENT = "svcTransient";
	public static final String RUNTIME_PATH = "target/svcTestRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/svctest"; //$NON-NLS-1$

	protected static final Logger logger = LoggerFactory.getLogger(AbstractRealmServiceTest.class);

	protected static RuntimeMock runtimeMock;
	protected Certificate certificate;

	protected String getUsername() {
		return "test";
	}

	@Before
	public void before() throws Exception {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");
		dropSchema("jdbc:postgresql://localhost/cacheduserauditsversioningdb", "cacheduserauditsversioning", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		this.certificate = runtimeMock.getPrivilegeHandler().authenticate(getUsername(), getUsername().toCharArray());
		importFromXml(REALM_CACHED, this.certificate, getServiceHandler());
		importFromXml(REALM_CACHED_AUDITS_VERSIONING, this.certificate, getServiceHandler());
	}

	@After
	public void after() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	public static void dropSchema(String dbUrl, String dbUsername, String dbPassword) throws Exception {

		if (!Driver.isRegistered())
			Driver.register();

		Version dbVersion = DbSchemaVersionCheck
				.getExpectedDbVersion(PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH, PostgreSqlPersistenceHandler.class);
		String sql = DbSchemaVersionCheck
				.getSql(PostgreSqlPersistenceHandler.SCRIPT_PREFIX_STROLCH, PostgreSqlPersistenceHandler.class, dbVersion,
						"drop"); //$NON-NLS-1$
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
		assertServiceResult(ServiceResultState.SUCCESS, XmlImportModelResult.class, result);
	}

	private void doService(String realm, ServiceResultState expectedState, Class<?> expectedServiceResultType,
			Runner before, Runner validator, Runner after) throws IllegalAccessException, InstantiationException {

		Service<T, U> svc = getSvcClass().newInstance();
		T arg = getArgInstance();

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
		void run(StrolchRealm strolchRealm, ComponentContainer container);
	}

	protected abstract Class<? extends Service<T, U>> getSvcClass();

	protected abstract T getArgInstance();

	protected void runServiceInAllRealmTypes() {
		runServiceInAllRealmTypes(null, null, null);
	}

	protected void runServiceInAllRealmTypes(Class<?> expectedServiceResultType) {
		runServiceInAllRealmTypes(expectedServiceResultType, null, null, null);
	}

	protected void runServiceInAllRealmTypes(Runner before, Runner validator, Runner after) {
		runServiceInAllRealmTypes(ServiceResult.class, before, validator, after);
	}

	protected void runServiceInAllRealmTypes(Class<?> expectedServiceResultType, Runner before, Runner validator,
			Runner after) {
		try {

			runTransient(expectedServiceResultType, before, validator, after);
			runCached(expectedServiceResultType, before, validator, after);
			runCachedWithAuditsAndVersioning(expectedServiceResultType, before, validator, after);
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException("Failed to instantiate class " + getSvcClass().getName() + ": " + e.getMessage(),
					e);
		}
	}

	private void runCached(Class<?> expectedServiceResultType, Runner before, Runner validator, Runner after)
			throws InstantiationException, IllegalAccessException {
		doService(REALM_CACHED, ServiceResultState.SUCCESS, expectedServiceResultType, before, validator, after);
	}

	private void runCachedWithAuditsAndVersioning(Class<?> expectedServiceResultType, Runner before, Runner validator,
			Runner after) throws InstantiationException, IllegalAccessException {
		doService(REALM_CACHED_AUDITS_VERSIONING, ServiceResultState.SUCCESS, expectedServiceResultType, before,
				validator, after);
	}

	protected void runTransient() throws IllegalAccessException, InstantiationException {
		runTransient(null);
	}

	protected void runTransient(Class<?> expectedServiceResultType)
			throws IllegalAccessException, InstantiationException {
		runTransient(expectedServiceResultType, null, null, null);
	}

	private void runTransient(Class<?> expectedServiceResultType, Runner before, Runner validator, Runner after)
			throws InstantiationException, IllegalAccessException {
		doService(REALM_TRANSIENT, ServiceResultState.SUCCESS, expectedServiceResultType, before, validator, after);
	}
}
