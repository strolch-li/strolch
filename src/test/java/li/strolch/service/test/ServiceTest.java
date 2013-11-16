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

import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;
import li.strolch.service.test.GreetingService.GreetingArgument;

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
		this.thrown.expect(PrivilegeException.class);
		TestService testService = new TestService();
		serviceHandler.doService(null, testService);
	}

	@Test
	public void shouldFailInvalidCertificate1() {
		this.thrown.expect(PrivilegeException.class);
		TestService testService = new TestService();
		serviceHandler.doService(new Certificate(null, 0, null, null, null, null, null), testService);
	}

	@Test
	public void shouldFailInvalidCertificate2() {
		this.thrown.expect(AccessDeniedException.class);
		TestService testService = new TestService();
		Certificate badCert = new Certificate("1", System.currentTimeMillis(), "bob", "dsdf", "dferg", null, null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		serviceHandler.doService(badCert, testService);
	}

	@Test
	public void shouldFailWithNoAccess() {
		this.thrown.expect(AccessDeniedException.class);
		this.thrown.expectMessage("User jill does not have Privilege li.strolch.service.Service"); //$NON-NLS-1$

		Certificate certificate = getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			serviceHandler.doService(certificate, testService);
		} finally {
			getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithAccess() {
		Certificate certificate = getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Jill"; //$NON-NLS-1$
			GreetingResult greetingResult = serviceHandler.doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Jill. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin1() {

		Certificate certificate = getPrivilegeHandler().authenticate("bob", "bob".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			TestService testService = new TestService();
			serviceHandler.doService(certificate, testService);
		} finally {
			getPrivilegeHandler().invalidateSession(certificate);
		}
	}

	@Test
	public void shouldNotFailWithLogin2() {
		Certificate certificate = getPrivilegeHandler().authenticate("bob", "bob".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingService service = new GreetingService();
			GreetingArgument argument = new GreetingArgument();
			argument.name = "Bob"; //$NON-NLS-1$
			GreetingResult greetingResult = serviceHandler.doService(certificate, service, argument);
			assertThat(greetingResult.getGreeting(), equalTo("Hello Bob. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			getPrivilegeHandler().invalidateSession(certificate);
		}
	}
}
