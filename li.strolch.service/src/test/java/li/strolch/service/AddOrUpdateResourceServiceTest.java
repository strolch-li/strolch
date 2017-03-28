/*
 * Copyright 2017 Reto Breitenmoser <reto.breitenmoser@gmail.com>
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

import org.junit.Test;

import li.strolch.model.ModelGenerator;
import li.strolch.service.AddOrUpdateResourceService.AddOrUpdateResourceArg;
import li.strolch.service.test.AbstractRealmServiceTest;

/**
 * Reto Breitenmoser <reto.breitenmoser@gmail.com>
 */
public class AddOrUpdateResourceServiceTest extends AbstractRealmServiceTest {

	@Test
	public void runTest() {

		AddOrUpdateResourceArg arg = new AddOrUpdateResourceArg();
		arg.resource = ModelGenerator.createResource("firstRes", "First Resource", "AdditionalResources");

		runServiceInAllRealmTypes(AddOrUpdateResourceService.class, arg);
		// run again for checking updates
		runServiceInAllRealmTypes(AddOrUpdateResourceService.class, arg);
	}
}
