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
package li.strolch.runtime.query.inmemory;

import static org.junit.Assert.assertNotNull;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Locator;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class FindByLocatorTest {

	private static final String PATH_FIND_BY_LOCATOR_RUNTIME = "target/FindByLocatorTest/";

	@Test
	public void shouldFindByLocator() {

		StrolchAgent agent = ComponentContainerTest.startContainer(PATH_FIND_BY_LOCATOR_RUNTIME,
				ComponentContainerTest.PATH_TRANSIENT_CONTAINER);
		ComponentContainer container = agent.getContainer();

		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx()) {
			Locator locResStringParam = Locator.valueOf("Resource/TestType/MyTestResource/@bag01/@param5");
			StringParameter resStringParam = tx.findElement(locResStringParam);
			assertNotNull("Should have found a StringParameter with the locator " + locResStringParam, resStringParam);

			Locator locOrderFloatParam = Locator.valueOf("Order/TestType/MyTestOrder/@bag01/@param2");
			FloatParameter orderFloatP = tx.findElement(locOrderFloatParam);
			assertNotNull("Should have found a FloatParameter with the locator " + locOrderFloatParam, orderFloatP);

			Locator locOrderBag = Locator.valueOf("Order/TestType/MyTestOrder/@bag01");
			ParameterBag orderBag = tx.findElement(locOrderBag);
			assertNotNull("Should have found a FloatParameter with the locator " + locOrderBag, orderBag);

			Locator locResource = Locator.valueOf("Resource/TestType/MyTestResource");
			Resource resource = tx.findElement(locResource);
			assertNotNull("Should have found a FloatParameter with the locator " + locResource, resource);
		}
	}
}
