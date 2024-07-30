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
package li.strolch.service.test.model;

import java.text.MessageFormat;

import li.strolch.service.api.AbstractService;
import li.strolch.service.api.ServiceArgument;
import li.strolch.service.test.model.GreetingService.GreetingArgument;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class GreetingService extends AbstractService<GreetingArgument, GreetingResult> {

	@Override
	protected GreetingResult internalDoService(GreetingArgument argument) {

		if (StringHelper.isEmpty(argument.name))
			throw new IllegalArgumentException("The name must always be set!");

		GreetingResult greetingResult = new GreetingResult();
		String greeting = MessageFormat.format("Hello {0}. Nice to meet you!", argument.name);
		greetingResult.setGreeting(greeting);
		return greetingResult;
	}

	@Override
	protected GreetingResult getResultInstance() {
		return new GreetingResult();
	}

	@Override
	public GreetingArgument getArgumentInstance() {
		return new GreetingArgument();
	}

	public static class GreetingArgument extends ServiceArgument {

		public String name;
	}
}
