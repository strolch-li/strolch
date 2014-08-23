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

import li.strolch.model.ModelGenerator;
import li.strolch.model.Resource;
import li.strolch.service.AddResourceCollectionService.AddResourceCollectionArg;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AddResourceCollectionServiceTest extends AbstractRealmServiceTest {

	@Test
	public void runTest() {

		AddResourceCollectionArg arg = new AddResourceCollectionArg();

		ArrayList<Resource> resources = new ArrayList<>();
		resources.add(ModelGenerator.createResource("firstRes", "First Resource", "AdditionalResources"));
		resources.add(ModelGenerator.createResource("secondRes", "Second Resource", "AdditionalResources"));
		resources.add(ModelGenerator.createResource("thirdRes", "Third Resource", "AdditionalResources"));

		arg.resources = resources;

		runServiceInAllRealmTypes(AddResourceCollectionService.class, arg);
	}
}
