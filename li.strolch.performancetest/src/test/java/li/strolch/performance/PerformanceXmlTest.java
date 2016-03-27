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

import li.strolch.testbase.runtime.RuntimeMock;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PerformanceXmlTest extends PerformanceTest {

	public static final String RUNTIME_PATH = "target/runtime_xml_test/"; //$NON-NLS-1$
	public static final String DB_STORE_PATH_DIR = "dbStore"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/runtime_xml"; //$NON-NLS-1$

	protected static RuntimeMock runtimeMock;

	@Override
	protected RuntimeMock runtime() {
		return runtimeMock;
	}

	@BeforeClass
	public static void beforeClass() throws Exception {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		new File(rootPath, DB_STORE_PATH_DIR).mkdir();
		runtimeMock.startContainer();
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}
}
