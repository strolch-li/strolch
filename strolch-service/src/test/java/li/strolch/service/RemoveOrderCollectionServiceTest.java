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
package li.strolch.service;

import java.util.ArrayList;

import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.service.api.Service;
import li.strolch.service.api.ServiceResult;
import li.strolch.service.test.AbstractRealmServiceTest;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveOrderCollectionServiceTest extends AbstractRealmServiceTest<LocatorListArgument, ServiceResult> {

	@Test
	public void runTest() {

		runServiceInAllRealmTypes();
	}

	@Override
	protected Class<? extends Service<LocatorListArgument, ServiceResult>> getSvcClass() {
		return RemoveOrdersService.class;
	}

	@Override
	protected LocatorListArgument getArgInstance() {

		LocatorListArgument arg = new LocatorListArgument();

		ArrayList<Locator> locators = new ArrayList<>();
		locators.add(Locator.newBuilder(Tags.ORDER, "TestType", "@1").build());
		locators.add(Locator.newBuilder(Tags.ORDER, "TestType", "@2").build());
		locators.add(Locator.newBuilder(Tags.ORDER, "TestType", "@3").build());

		arg.locators = locators;

		return arg;
	}
}
