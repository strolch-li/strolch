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

import static li.strolch.service.test.AbstractRealmServiceTest.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertTrue;

import java.io.File;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.Command;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractRealmCommandTest {

	protected static RuntimeMock runtimeMock;

	protected static Certificate certificate;

	protected String getUsername() {
		return "test";
	}

	@Before
	public void beforeClass() throws Exception {

		dropSchema("jdbc:postgresql://localhost/cacheduserdb", "cacheduser", "test");

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		certificate = runtimeMock.getPrivilegeHandler().authenticate(getUsername(), getUsername().toCharArray());
		importFromXml(REALM_CACHED, certificate, getServiceHandler());
	}

	@After
	public void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}

	protected abstract Command getCommandInstance(ComponentContainer container, StrolchTransaction tx);

	protected abstract void validateAfterCommand(ComponentContainer container, StrolchTransaction tx);

	protected abstract void validateAfterCommandFailed(ComponentContainer container, StrolchTransaction tx);

	protected void doCommandAsFail(String realmName) {
		StrolchRealm realm = runtimeMock.getContainer().getRealm(realmName);
		boolean caught = false;
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			Command command = getCommandInstance(runtimeMock.getContainer(), tx);
			FailCommandFacade commandFacade = new FailCommandFacade(runtimeMock.getContainer(), tx, command);

			tx.addCommand(commandFacade);
			tx.commitOnClose();
		} catch (RuntimeException e) {
			assertThat(e.getMessage(), containsString("Fail on purpose after do command!"));
			caught = true;
		}
		assertTrue(caught);

		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			validateAfterCommandFailed(runtimeMock.getContainer(), tx);
		}
	}

	protected void doCommand(String realmName) {
		StrolchRealm realm = runtimeMock.getContainer().getRealm(realmName);
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			Command command = getCommandInstance(runtimeMock.getContainer(), tx);
			tx.addCommand(command);
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			validateAfterCommand(runtimeMock.getContainer(), tx);
		}
	}

	@Test
	public void shouldDoCommandTransient() {
		doCommandAsFail(REALM_TRANSIENT);
		doCommand(REALM_TRANSIENT);
	}

	@Test
	public void shouldDoCommandCached() {
		doCommandAsFail(REALM_CACHED);
		doCommand(REALM_CACHED);
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
