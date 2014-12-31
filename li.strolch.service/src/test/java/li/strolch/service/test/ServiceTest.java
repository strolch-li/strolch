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
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertThat;

import java.util.HashSet;

import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.model.GreetingResult;
import li.strolch.service.test.model.GreetingService;
import li.strolch.service.test.model.GreetingService.GreetingArgument;
import li.strolch.service.test.model.TestService;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ServiceTest extends AbstractServiceTest {

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	@Test
	public void shouldFailNoCertificate() {
		TestService testService = new TestService();
		ServiceResult svcResult = getServiceHandler().doService(null, testService);
		assertThat(svcResult.getThrowable(), instanceOf(PrivilegeException.class));
	}

	@Test
	public void shouldFailInvalidCertificate1() {
		this.thrown.expect(PrivilegeException.class);
		TestService testService = new TestService();
		getServiceHandler().doService(
				new Certificate(null, 0, null, null, null, null, null, new HashSet<String>(), null), testService);
	}

	@Test
	public void shouldFailInvalidCertificate2() {
		TestService testService = new TestService();
		Certificate badCert = new Certificate(
				"1", System.currentTimeMillis(), "bob", "Bob", "Brown", "dsdf", null, new HashSet<String>(), null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ 
		ServiceResult svcResult = getServiceHandler().doService(badCert, testService);
		assertThat(svcResult.getThrowable(), instanceOf(AccessDeniedException.class));
	}

	@Test
	public void shouldFailWithNoAccess() {

		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			ServiceResult svcResult = getServiceHandler().doService(certificate, testService);
			assertThat(svcResult.getMessage(),
					containsString("User jill does not have Privilege li.strolch.service.api.Service")); //$NON-NLS-1$
			assertThat(svcResult.getThrowable(), instanceOf(AccessDeniedException.class));
		} finally {
			runtimeMock.getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithAccess() {
		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Jill"; //$NON-NLS-1$
			GreetingResult greetingResult = getServiceHandler().doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Jill. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			runtimeMock.getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin1() {

		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("bob", "bob".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			getServiceHandler().doService(certificate, testService);
		} finally {
			runtimeMock.getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin2() {
		Certificate certificate = runtimeMock.getPrivilegeHandler().authenticate("bob", "bob".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Bob"; //$NON-NLS-1$
			GreetingResult greetingResult = getServiceHandler().doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Bob. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			runtimeMock.getPrivilegeHandler().invalidateSession(certificate);
		}
	}
}
