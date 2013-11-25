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
import li.strolch.service.test.GreetingService.GreetingArgument;

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

		Certificate certificate = getPrivilegeHandler().authenticate("jill", "jill".getBytes()); //$NON-NLS-1$//$NON-NLS-2$
		try {
			GreetingResult greetingResult = serviceHandler.doService(certificate, greetingService, greetingArgument);
			assertThat(greetingResult.getGreeting(), containsString("Hello Robert. Nice to meet you!")); //$NON-NLS-1$
		} finally {
			getPrivilegeHandler().invalidateSession(certificate);
		}
	}
}
