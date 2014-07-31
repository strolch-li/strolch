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
import li.strolch.service.RemoveResourceCollectionService.RemoveResourceCollectionArg;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveResourceCollectionServiceTest extends AbstractRealmServiceTest {

	@Test
	public void runTest() {

		RemoveResourceCollectionService svc = new RemoveResourceCollectionService();
		RemoveResourceCollectionArg arg = new RemoveResourceCollectionArg();

		ArrayList<Locator> locators = new ArrayList<>();
		locators.add(Locator.newBuilder(Tags.RESOURCE).append("Enumeration").append("salutations").build());
		locators.add(Locator.newBuilder(Tags.RESOURCE).append("Enumeration").append("sex").build());
		locators.add(Locator.newBuilder(Tags.RESOURCE).append("Enumeration").append("religions").build());

		arg.locators = locators;

		runServiceInAllRealmTypes(svc, arg);
	}
}
