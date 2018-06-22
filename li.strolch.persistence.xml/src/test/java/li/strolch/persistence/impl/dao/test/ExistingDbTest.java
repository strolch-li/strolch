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
package li.strolch.persistence.impl.dao.test;

import static org.junit.Assert.assertNotNull;

import java.io.File;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.helper.FileHelper;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ExistingDbTest {

	private static final String TEST = "test"; //$NON-NLS-1$
	public static final String RUNTIME_PATH = "target/existingDbRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/existingDbRuntime"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);

		File dbStoreSrcPath = new File(CONFIG_SRC, "data/dbStore");
		File dbStoreDstPath = new File(RUNTIME_PATH, "data/dbStore");
		if (!dbStoreDstPath.exists() && !dbStoreDstPath.mkdir()) {
			throw new RuntimeException("Could not create db store " + dbStoreDstPath.getAbsolutePath());
		}
		if (!FileHelper.copy(dbStoreSrcPath.listFiles(), dbStoreDstPath, false)) {
			throw new RuntimeException("Failed to copy db store from " + dbStoreSrcPath + " to " + dbStoreDstPath);
		}

		runtimeMock.startContainer();
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	@Test
	public void shouldQueryExistingData() {

		PrivilegeHandler privilegeHandler = runtimeMock.getAgent().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler.authenticate(TEST, TEST.toCharArray());

		try (StrolchTransaction tx = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, TEST)) {
			Resource resource = tx.getResourceBy("MyType", "@1"); //$NON-NLS-1$ //$NON-NLS-2$
			assertNotNull("Should be able to read existing element from db", resource); //$NON-NLS-1$

			Order order = tx.getOrderBy("MyType", "@1"); //$NON-NLS-1$//$NON-NLS-2$
			assertNotNull("Should be able to read existing element from db", order); //$NON-NLS-1$
		}
	}
}
