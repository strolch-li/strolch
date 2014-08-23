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
package li.strolch.command;

import static li.strolch.service.test.AbstractRealmServiceTest.CONFIG_SRC;
import static li.strolch.service.test.AbstractRealmServiceTest.REALM_CACHED;
import static li.strolch.service.test.AbstractRealmServiceTest.REALM_TRANSACTIONAL;
import static li.strolch.service.test.AbstractRealmServiceTest.REALM_TRANSIENT;
import static li.strolch.service.test.AbstractRealmServiceTest.RUNTIME_PATH;
import static li.strolch.service.test.AbstractRealmServiceTest.dropSchema;
import static li.strolch.service.test.AbstractRealmServiceTest.importFromXml;

import java.io.File;
import java.sql.SQLException;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRealmCommandTest {

	protected static RuntimeMock runtimeMock;

	protected static Certificate certificate;

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@BeforeClass
	public static void beforeClass() throws SQLException {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");
		dropSchema("jdbc:postgresql://localhost/transactionaluserdb", "transactionaluser", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".getBytes());
		importFromXml(REALM_CACHED, certificate, getServiceHandler());
		importFromXml(REALM_TRANSACTIONAL, certificate, getServiceHandler());
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}

	protected abstract Command getCommandInstance(ComponentContainer container, StrolchTransaction tx);

	protected void doCommandAsFail(String realmName) {
		expectedException.expect(RuntimeException.class);
		expectedException.expectMessage("Fail on purpose after do command!");

		StrolchRealm realm = runtimeMock.getContainer().getRealm(realmName);
		try (StrolchTransaction tx = realm.openTx(certificate, "test")) {

			Command command = getCommandInstance(runtimeMock.getContainer(), tx);

			FailCommandFacade commandFacade = new FailCommandFacade(runtimeMock.getContainer(), tx, command);
			tx.addCommand(commandFacade);
		}
	}

	protected void doCommand(String realmName) {
		StrolchRealm realm = runtimeMock.getContainer().getRealm(realmName);
		try (StrolchTransaction tx = realm.openTx(certificate, "test")) {
			Command command = getCommandInstance(runtimeMock.getContainer(), tx);
			tx.addCommand(command);
		}
	}

	@Test
	public void shouldFailCommandTransient() {
		doCommandAsFail(REALM_TRANSIENT);
	}

	@Test
	public void shouldFailCommandCached() {
		doCommandAsFail(REALM_CACHED);
	}

	@Test
	public void shouldFailCommandTransactional() {
		doCommandAsFail(REALM_TRANSACTIONAL);
	}

	private class FailCommandFacade extends Command {

		private Command command;

		/**
		 * @param container
		 * @param tx
		 */
		public FailCommandFacade(ComponentContainer container, StrolchTransaction tx, Command command) {
			super(container, tx);
			this.command = command;
		}

		@Override
		public void validate() {
			this.command.validate();
		}

		@Override
		public void doCommand() {
			this.command.doCommand();
			throw new RuntimeException("Fail on purpose after do command!");
		}

		@Override
		public void undo() {
			this.command.undo();
		}
	}
}
