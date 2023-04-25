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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThrows;

import java.time.ZonedDateTime;
import java.util.HashSet;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.base.NotAuthenticatedException;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.UserState;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.model.GreetingResult;
import li.strolch.service.test.model.GreetingService;
import li.strolch.service.test.model.GreetingService.GreetingArgument;
import li.strolch.service.test.model.TestService;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceTest extends AbstractServiceTest {

	@Test
	public void shouldFailInvalidCertificate1() {
		assertThrows(PrivilegeException.class, () -> {
			TestService testService = new TestService();
			getServiceHandler().doService(
					new Certificate(null, null, null, null, null, null, null, null, ZonedDateTime.now(), false, null,
							new HashSet<>(), null), testService);
		});
	}

	@Test
	public void shouldFailInvalidCertificate2() {
		TestService testService = new TestService();
		Certificate badCert = new Certificate(Usage.ANY, "1", "bob", "Bob", "Brown", UserState.ENABLED, "dsdf", "asd",
				ZonedDateTime.now(), false, null, new HashSet<>(), null);
		ServiceResult svcResult = getServiceHandler().doService(badCert, testService);
		assertThat(svcResult.getThrowable(), instanceOf(NotAuthenticatedException.class));
	}

	@Test
	public void shouldFailWithNoAccess() {

		Certificate certificate = runtimeMock.getPrivilegeHandler()
				.authenticate("jill", "jill".toCharArray());//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			ServiceResult svcResult = getServiceHandler().doService(certificate, testService);
			assertThat(svcResult.getMessage(),
					containsString("User jill may not perform service TestService"));
			assertThat(svcResult.getThrowable(), instanceOf(AccessDeniedException.class));
		} finally {
			runtimeMock.getPrivilegeHandler().invalidate(certificate);
		}
	}

	@Test
	public void shouldNotFailWithAccess() {
		Certificate certificate = runtimeMock.getPrivilegeHandler()
				.authenticate("jill", "jill".toCharArray());//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Jill";
			GreetingResult greetingResult = getServiceHandler().doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Jill. Nice to meet you!"));
		} finally {
			runtimeMock.getPrivilegeHandler().invalidate(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin1() {

		Certificate certificate = runtimeMock.getPrivilegeHandler()
				.authenticate("bob", "bob".toCharArray());//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			getServiceHandler().doService(certificate, testService);
		} finally {
			runtimeMock.getPrivilegeHandler().invalidate(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin2() {
		Certificate certificate = runtimeMock.getPrivilegeHandler()
				.authenticate("bob", "bob".toCharArray());//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Bob";
			GreetingResult greetingResult = getServiceHandler().doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Bob. Nice to meet you!"));
		} finally {
			runtimeMock.getPrivilegeHandler().invalidate(certificate);
		}
	}
}
