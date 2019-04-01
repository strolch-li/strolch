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

import static org.junit.Assert.assertTrue;

import java.io.File;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.AbstractModelTest;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.AfterClass;
import org.junit.BeforeClass;

public class XmlCachedDaoTest extends AbstractModelTest {

	public static final String RUNTIME_PATH = "target/cachedStrolchRuntime/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock getRuntimeMock() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();

		// check that the data was initialized
		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
		try (StrolchTransaction tx = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM)
				.openTx(certificate, "test", true)) {
			assertTrue("Model was not properly initialized!", tx.hasResource("Template", "TestType"));
			assertTrue("Model was not properly initialized!", tx.hasOrder("Template", "MyTestOrder"));
		}
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}
}
