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

import li.strolch.persistence.postgresql.DataType;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PerformancePostgreSqlJsonTest extends PerformanceTest {

	public static final String RUNTIME_PATH = "target/runtime_postgresql_test/"; //$NON-NLS-1$
	public static final String CONFIG_SRC = "src/runtime_postgresql_json"; //$NON-NLS-1$

	public static final String DB_URL = "jdbc:postgresql://localhost/testdb"; //$NON-NLS-1$
	public static final String DB_USERNAME = "testuser"; //$NON-NLS-1$
	public static final String DB_PASSWORD = "test"; //$NON-NLS-1$

	@BeforeClass
	public static void beforeClass() throws Exception {
		dropSchema(PerformancePostgreSqlJsonTest.class.getSimpleName(), DB_URL, DB_USERNAME, DB_PASSWORD);
		buildRuntime(CONFIG_SRC, RUNTIME_PATH, DataType.json);
	}

	@AfterClass
	public static void afterClass() throws Exception {
		afterClass(RUNTIME_PATH);
	}

	@Test
	public void runPerformanceTestCached() {
		runPerformanceTest("cached", 1);
	}

	@Test
	public void runPerformanceTestBulk() {
		runPerformanceTest("cached", 20);
	}

	@Test
	public void runParallelPerformanceTest() {
		runParallelPerformanceTest("cached", 1);
	}
}
