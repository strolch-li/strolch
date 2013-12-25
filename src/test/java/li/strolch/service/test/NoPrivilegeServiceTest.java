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

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertThat;

import java.io.File;

import li.strolch.service.ServiceHandler;
import li.strolch.service.test.GreetingService.GreetingArgument;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class NoPrivilegeServiceTest {

	private static final String RUNTIME_PATH = "target/strolchRuntime/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/withoutPrivilegeRuntime/config"; //$NON-NLS-1$
	protected static RuntimeMock runtimeMock;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer(rootPath);
	}

	@AfterClass
	public static void afterClass() {
		runtimeMock.destroyRuntime();
	}

	public static ServiceHandler getServiceHandler() {
		return runtimeMock.getContainer().getComponent(ServiceHandler.class);
	}

	@Test
	public void shouldPerformSimpleService() {

		GreetingService greetingService = new GreetingService();
		GreetingArgument greetingArgument = new GreetingArgument();
		greetingArgument.name = "Robert"; //$NON-NLS-1$

		GreetingResult greetingResult = getServiceHandler().doService(null, greetingService, greetingArgument);
		assertThat(greetingResult.getGreeting(), containsString("Hello Robert. Nice to meet you!")); //$NON-NLS-1$
	}
}
