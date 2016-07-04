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

import org.junit.Test;

import li.strolch.RuntimeMock;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class FindByLocatorTest {

	private static final String PATH_FIND_BY_LOCATOR_RUNTIME = "target/FindByLocatorTest/";

	@Test
	public void shouldFindByLocator() {
		RuntimeMock.runInStrolch(PATH_FIND_BY_LOCATOR_RUNTIME, ComponentContainerTest.PATH_TRANSIENT_CONTAINER,
				agent -> doLocatorTest(agent));
	}

	private void doLocatorTest(StrolchAgent agent) {
		ComponentContainer container = agent.getContainer();

		Certificate certificate = container.getPrivilegeHandler().authenticate("test", "test".getBytes());

		try (StrolchTransaction tx = container.getRealm(StrolchConstants.DEFAULT_REALM).openTx(certificate, "test")) {

			// Resource
			Locator locResource = Locator.valueOf("Resource/TestType/MyTestResource");
			Resource resource = tx.findElement(locResource);
			assertNotNull("Should have found a FloatParameter with the locator " + locResource, resource);

			// Order
			Locator locOrder = Locator.valueOf("Order/TestType/MyTestOrder");
			Order order = tx.findElement(locOrder);
			assertNotNull("Should have found an Order with the locator " + locOrder, order);

			// Bag on Resource
			Locator locResBag = Locator.valueOf("Resource/TestType/MyTestResource/Bag/@bag01");
			ParameterBag resBag = tx.findElement(locResBag);
			assertNotNull("Should have found a ParameterBag with the locator " + locResBag, resBag);

			// Bag on Order
			Locator locOrdBag = Locator.valueOf("Order/TestType/MyTestOrder/Bag/@bag01");
			ParameterBag ordBag = tx.findElement(locOrdBag);
			assertNotNull("Should have found a ParameterBag with the locator " + ordBag, locOrdBag);

			// Parameter on Resource
			Locator locResStringParam = Locator.valueOf("Resource/TestType/MyTestResource/Bag/@bag01/@param5");
			StringParameter resStringParam = tx.findElement(locResStringParam);
			assertNotNull("Should have found a StringParameter with the locator " + locResStringParam, resStringParam);

			// Parameter on Order
			Locator locOrderFloatParam = Locator.valueOf("Order/TestType/MyTestOrder/Bag/@bag01/@param2");
			FloatParameter orderFloatP = tx.findElement(locOrderFloatParam);
			assertNotNull("Should have found a FloatParameter with the locator " + locOrderFloatParam, orderFloatP);

			// TimedState on Resource
			Locator locResIntegerState = Locator.valueOf("Resource/TestType/MyTestResource/State/@integerState");
			IntegerTimedState integerS = tx.findElement(locResIntegerState);
			assertNotNull("Should have found a IntegerTimedState with the locator " + locResIntegerState, integerS);
		}
	}
}
