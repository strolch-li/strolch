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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import li.strolch.RuntimeMock;
import li.strolch.agent.ComponentContainerTest;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class FindByLocatorTest {

	private static final String PATH_FIND_BY_LOCATOR_RUNTIME = "target/FindByLocatorTest/";

	private static RuntimeMock runtimeMock;
	private static Certificate certificate;

	@BeforeClass
	public static void beforeClass() {
		runtimeMock = new RuntimeMock(PATH_FIND_BY_LOCATOR_RUNTIME, ComponentContainerTest.PATH_TRANSIENT_CONTAINER);
		runtimeMock.mockRuntime();
		runtimeMock.startContainer();
		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());

	}

	@Test
	public void shouldFindByResource() throws Exception {
		runtimeMock.run(agent -> {
			try (StrolchTransaction tx = agent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test", true)) {

				// Resource
				Locator locResource = Locator.valueOf("Resource/TestType/MyTestResource");
				Resource resource = tx.findElement(locResource);
				assertNotNull("Should have found a FloatParameter with the locator " + locResource, resource);

				// Bag on Resource
				Locator locResBag = Locator.valueOf("Resource/TestType/MyTestResource/Bag/@bag01");
				ParameterBag resBag = tx.findElement(locResBag);
				assertNotNull("Should have found a ParameterBag with the locator " + locResBag, resBag);

				// Parameter on Resource
				Locator locResStringParam = Locator.valueOf("Resource/TestType/MyTestResource/Bag/@bag01/@param5");
				StringParameter resStringParam = tx.findElement(locResStringParam);
				assertNotNull("Should have found a StringParameter with the locator " + locResStringParam,
						resStringParam);

				// TimedState on Resource
				Locator locResIntegerState = Locator
						.valueOf("Resource/TestType/MyTestResource/TimedState/@integerState");
				IntegerTimedState integerS = tx.findElement(locResIntegerState);
				assertNotNull("Should have found a IntegerTimedState with the locator " + locResIntegerState, integerS);

			}
		});
	}

	@Test
	public void shouldFindByOrder() throws Exception {
		runtimeMock.run(agent -> {
			try (StrolchTransaction tx = agent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test", true)) {

				// Order
				Locator locOrder = Locator.valueOf("Order/TestType/MyTestOrder");
				Order order = tx.findElement(locOrder);
				assertNotNull("Should have found an Order with the locator " + locOrder, order);

				// Bag on Order
				Locator locOrdBag = Locator.valueOf("Order/TestType/MyTestOrder/Bag/@bag01");
				ParameterBag ordBag = tx.findElement(locOrdBag);
				assertNotNull("Should have found a ParameterBag with the locator " + ordBag, locOrdBag);

				// Parameter on Order
				Locator locOrderFloatParam = Locator.valueOf("Order/TestType/MyTestOrder/Bag/@bag01/@param2");
				FloatParameter orderFloatP = tx.findElement(locOrderFloatParam);
				assertNotNull("Should have found a FloatParameter with the locator " + locOrderFloatParam, orderFloatP);

			}
		});
	}

	@Test
	public void shouldFindByActivity() throws Exception {
		runtimeMock.run(agent -> {
			try (StrolchTransaction tx = agent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test", true)) {

				// Activity
				Locator locActivity = Locator.valueOf("Activity/ActivityType/activity_1");
				Activity activity = tx.findElement(locActivity);
				assertNotNull("Should have found Activity with id activity_1", activity);
				assertEquals("activity_1", activity.getId());
				assertEquals(locActivity.toString(), activity.getLocator().toString());

				// sub activity
				Locator locAction = Locator.valueOf("Activity/ActivityType/activity_1/child_activity");
				activity = tx.findElement(locAction);
				assertNotNull("Should have found sub Activity with id child_activity", activity);
				assertEquals("child_activity", activity.getId());
				assertEquals(locAction.toString(), activity.getLocator().toString());

			}
		});
	}

	@Test
	public void shouldFindByAction() throws Exception {
		runtimeMock.run(agent -> {
			try (StrolchTransaction tx = agent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM)
					.openTx(certificate, "test", true)) {

				// sub action
				Locator locAction = Locator.valueOf("Activity/ActivityType/activity_1/action_1");
				Action action = tx.findElement(locAction);
				assertNotNull("Should have found Action with id action_1 on Activity activity_1", action);
				assertEquals("action_1", action.getId());
				assertEquals(locAction.toString(), action.getLocator().toString());

				// sub sub action
				locAction = Locator.valueOf("Activity/ActivityType/activity_1/child_activity/action_2");
				action = tx.findElement(locAction);
				assertNotNull("Should have found sub Activity with id child_activity", action);
				assertEquals("action_2", action.getId());
				assertEquals(locAction.toString(), action.getLocator().toString());

				// sub sub action
				locAction = Locator.valueOf("Activity/ActivityType/activity_1/child_activity/action_3");
				action = tx.findElement(locAction);
				assertNotNull("Should have found sub Activity with id child_activity", action);
				assertEquals("action_3", action.getId());
				assertEquals(locAction.toString(), action.getLocator().toString());
			}
		});
	}
}
