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
package li.strolch.service.parameter;

import li.strolch.model.Locator;
import li.strolch.service.LocatorArgument;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveParameterServiceTest extends AbstractRealmServiceTest<LocatorArgument, ServiceResult> {

	@Test
	public void runTest() {
		runServiceInAllRealmTypes();
	}

	@Override
	protected Class<? extends Service<LocatorArgument, ServiceResult>> getSvcClass() {
		return RemoveParameterService.class;
	}

	@Override
	protected LocatorArgument getArgInstance() {
		LocatorArgument arg = new LocatorArgument();
		arg.locator = Locator.valueOf("Resource/Ball/yellow/Bag/parameters/owner");
		return arg;
	}
}
