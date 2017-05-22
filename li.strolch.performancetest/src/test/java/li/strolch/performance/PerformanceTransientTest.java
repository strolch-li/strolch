/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.performance;

import java.io.File;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.postgresql.Driver;

import li.strolch.privilege.model.Certificate;
import li.strolch.service.api.ServiceHandler;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PerformanceTransientTest {

	public static final String RUNTIME_PATH = "target/runtime_transient_test/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/runtime_transient"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() throws Exception {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();
	}

	@Test
	public void runPerformanceTest() {

		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("transient", "transient".toCharArray());

		ServiceHandler svcHandler = runtimeMock.getServiceHandler();
		svcHandler.doService(certificate, new PerformanceTestService(), new PerformanceTestArgument());
	}

	@AfterClass
	public static void afterClass() throws Exception {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();

		File rootPath = new File(RUNTIME_PATH);
		if (rootPath.exists()) {
			FileHelper.deleteFile(rootPath, false);
		}

		if (Driver.isRegistered())
			Driver.deregister();
	}
}
