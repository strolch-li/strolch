/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
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
public class NoPrivilegeServiceTest extends RuntimeMock {

	private static final String RUNTIME_PATH = "target/strolchRuntime/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/withoutPrivilegeRuntime/config"; //$NON-NLS-1$
	protected static ServiceHandler serviceHandler;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		RuntimeMock.mockRuntime(rootPath, configSrc);
		RuntimeMock.startContainer(rootPath);

		// initialize the component configuration
		serviceHandler = getContainer().getComponent(ServiceHandler.class);
	}

	@AfterClass
	public static void afterClass() {
		RuntimeMock.destroyRuntime();
	}

	@Test
	public void shouldPerformSimpleService() {

		GreetingService greetingService = new GreetingService();
		GreetingArgument greetingArgument = new GreetingArgument();
		greetingArgument.name = "Robert"; //$NON-NLS-1$

		GreetingResult greetingResult = serviceHandler.doService(null, greetingService, greetingArgument);
		assertThat(greetingResult.getGreeting(), containsString("Hello Robert. Nice to meet you!")); //$NON-NLS-1$
	}
}
