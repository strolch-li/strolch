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
import li.strolch.service.test.model.GreetingResult;
import li.strolch.service.test.model.GreetingService;
import li.strolch.service.test.model.GreetingService.GreetingArgument;

import org.junit.Test;

import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class GreetingServiceTest extends AbstractServiceTest {

	@Test
	public void shouldPerformSimpleService() {

		GreetingService greetingService = new GreetingService();
		GreetingArgument greetingArgument = new GreetingArgument();
		greetingArgument.name = "Robert"; //$NON-NLS-1$

		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingResult greetingResult = getServiceHandler().doService(certificate, greetingService,
					greetingArgument);
			assertThat(greetingResult.getGreeting(), containsString("Hello Robert. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			runtimeMock.getPrivilegeHandler().invalidateSession(certificate);
		}
	}
}
