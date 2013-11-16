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

import java.text.MessageFormat;

import li.strolch.service.AbstractService;
import li.strolch.service.ServiceArgument;
import li.strolch.service.test.GreetingService.GreetingArgument;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class GreetingService extends AbstractService<GreetingArgument, GreetingResult> {
	private static final long serialVersionUID = 1L;

	@Override
	protected GreetingResult internalDoService(GreetingArgument argument) {

		if (StringHelper.isEmpty(argument.name))
			throw new IllegalArgumentException("The name must always be set!"); //$NON-NLS-1$

		GreetingResult greetingResult = new GreetingResult();
		String greeting = MessageFormat.format("Hello {0}. Nice to meet you!", argument.name); //$NON-NLS-1$
		greetingResult.setGreeting(greeting);
		return greetingResult;
	}

	@Override
	protected GreetingResult getResultInstance() {
		return new GreetingResult();
	}

	public static class GreetingArgument extends ServiceArgument {
		private static final long serialVersionUID = 1L;
		public String name;
	}
}
