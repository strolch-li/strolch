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
package li.strolch.model;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.BAG_NAME;
import static li.strolch.model.ModelGenerator.BAG_TYPE;
import static li.strolch.model.ModelGenerator.PARAM_BOOLEAN_ID;
import static li.strolch.model.ModelGenerator.PARAM_DATE_ID;
import static li.strolch.model.ModelGenerator.PARAM_FLOAT_ID;
import static li.strolch.model.ModelGenerator.PARAM_INTEGER_ID;
import static li.strolch.model.ModelGenerator.PARAM_LIST_STRING_ID;
import static li.strolch.model.ModelGenerator.PARAM_LONG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;
import static li.strolch.model.ModelGenerator.STATE_BOOLEAN_ID;
import static li.strolch.model.ModelGenerator.STATE_BOOLEAN_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_BOOLEAN_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_BOOLEAN_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_BOOLEAN_TIME_30;
import static li.strolch.model.ModelGenerator.STATE_FLOAT_ID;
import static li.strolch.model.ModelGenerator.STATE_FLOAT_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_FLOAT_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_FLOAT_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_FLOAT_TIME_30;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_ID;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_INTEGER_TIME_30;
import static li.strolch.model.ModelGenerator.STATE_STRING_ID;
import static li.strolch.model.ModelGenerator.STATE_STRING_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_STRING_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_STRING_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_STRING_TIME_30;
import static li.strolch.model.ModelGenerator.STATE_TIME_0;
import static li.strolch.model.ModelGenerator.STATE_TIME_10;
import static li.strolch.model.ModelGenerator.STATE_TIME_20;
import static li.strolch.model.ModelGenerator.STATE_TIME_30;
import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static li.strolch.model.Tags.BAG;
import static li.strolch.model.Tags.ORDER;
import static li.strolch.model.Tags.RESOURCE;
import static li.strolch.model.Tags.STATE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Date;

import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;

import org.junit.Test;

@SuppressWarnings("nls")
public class ModelTest {

	@Test
	public void shouldCreateResource() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		ParameterBag bag = resource.getParameterBag(BAG_ID);
		validateBag(bag);
		validateStates(resource);
	}

	@Test
	public void shouldCreateOrder() {

		Order order = createOrder("@ord01", "Test Order", "MyType", new Date(), State.OPEN);
		ParameterBag bag = order.getParameterBag(BAG_ID);
		validateBag(bag);
	}

	@Test
	public void shouldCreateLocators() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		assertEquals(Locator.valueOf(RESOURCE, "MyType", "@res01"), resource.getLocator());
		ParameterBag bag = resource.getParameterBag(BAG_ID);
		assertEquals(Locator.valueOf(RESOURCE, "MyType", "@res01", BAG, BAG_ID), bag.getLocator());
		StringParameter sP = bag.getParameter(PARAM_STRING_ID);
		assertEquals(Locator.valueOf(RESOURCE, "MyType", "@res01", BAG, BAG_ID, PARAM_STRING_ID), sP.getLocator());
		FloatTimedState floatS = resource.getTimedState(STATE_FLOAT_ID);
		assertEquals(Locator.valueOf(RESOURCE, "MyType", "@res01", STATE, STATE_FLOAT_ID), floatS.getLocator());

		Order order = createOrder("@ord01", "Test Order", "MyType", new Date(), State.OPEN);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01"), order.getLocator());
		bag = order.getParameterBag(BAG_ID);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01", BAG, BAG_ID), bag.getLocator());
		sP = bag.getParameter(PARAM_STRING_ID);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01", BAG, BAG_ID, PARAM_STRING_ID), sP.getLocator());
	}

	@Test
	public void shouldPerformDeepResourceEquals() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertTrue("Same Resource should be deep equal!", visitor.isEqual());
	}

	@Test
	public void shouldFailDeepResourceEquals1() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		ParameterBag bag = dstRes.getParameterBag(BAG_ID);
		bag.setName("Bla bla");
		FloatParameter fParam = bag.getParameter(PARAM_FLOAT_ID);
		fParam.setValue(23434234.234);
		fParam.setName("Ohla");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertFalse("Resource should not be same if param is changed!", visitor.isEqual());
		assertEquals("Three changes should be registered", 3, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldFailDeepResourceEquals2() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		BooleanTimedState timedState = dstRes.getTimedState(STATE_BOOLEAN_ID);
		timedState.applyChange(new ValueChange<>(System.currentTimeMillis(), new BooleanValue(Boolean.TRUE)));
		timedState.setName("Ohla");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertFalse("Resource should not be same if param is changed!", visitor.isEqual());
		assertEquals("One change should be registered!", 1, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldPerformDeepOrderEquals() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.OPEN);
		Order dstOrder = createOrder("@ord01", "Test Order", "MyType", date, State.OPEN);
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		assertTrue("Same Order should be deep equal: " + visitor.getMismatchedLocators(), visitor.isEqual());
	}

	public static void validateBag(ParameterBag bag) {

		assertNotNull(bag);

		assertEquals(BAG_ID, bag.getId());
		assertEquals(BAG_NAME, bag.getName());
		assertEquals(BAG_TYPE, bag.getType());

		validateParams(bag);
	}

	public static void validateParams(ParameterBag bag) {

		BooleanParameter boolParam = bag.getParameter(PARAM_BOOLEAN_ID);
		assertNotNull("Boolean Param missing with id " + PARAM_BOOLEAN_ID, boolParam);
		assertEquals(true, boolParam.getValue().booleanValue());

		FloatParameter floatParam = bag.getParameter(PARAM_FLOAT_ID);
		assertNotNull("Float Param missing with id " + PARAM_FLOAT_ID, floatParam);
		assertEquals(44.3, floatParam.getValue().doubleValue(), 0.0001);

		IntegerParameter integerParam = bag.getParameter(PARAM_INTEGER_ID);
		assertNotNull("Integer Param missing with id " + PARAM_INTEGER_ID, integerParam);
		assertEquals(77, integerParam.getValue().intValue());

		LongParameter longParam = bag.getParameter(PARAM_LONG_ID);
		assertNotNull("Long Param missing with id " + PARAM_LONG_ID, longParam);
		assertEquals(4453234566L, longParam.getValue().longValue());

		StringParameter stringParam = bag.getParameter(PARAM_STRING_ID);
		assertNotNull("String Param missing with id " + PARAM_STRING_ID, stringParam);
		assertEquals("Strolch", stringParam.getValue());

		DateParameter dateParam = bag.getParameter(PARAM_DATE_ID);
		assertNotNull("Date Param missing with id " + PARAM_DATE_ID, dateParam);
		assertEquals(1354295525628L, dateParam.getValue().getTime());

		StringListParameter stringListP = bag.getParameter(PARAM_LIST_STRING_ID);
		assertNotNull("StringList Param missing with id " + PARAM_LIST_STRING_ID, stringListP);

		ArrayList<String> stringList = new ArrayList<>();
		stringList.add("Hello");
		stringList.add("World");
		assertEquals(stringList, stringListP.getValue());
	}

	/**
	 * @param resource
	 */
	private void validateStates(Resource resource) {
		BooleanTimedState booleanState = resource.getTimedState(STATE_BOOLEAN_ID);
		assertNotNull("Boolean State missing with id " + STATE_BOOLEAN_ID, booleanState);
		assertEquals(STATE_BOOLEAN_TIME_0, booleanState.getStateAt(STATE_TIME_0).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_10, booleanState.getStateAt(STATE_TIME_10).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_20, booleanState.getStateAt(STATE_TIME_20).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_30, booleanState.getStateAt(STATE_TIME_30).getValue().getValue());

		FloatTimedState floatState = resource.getTimedState(STATE_FLOAT_ID);
		assertNotNull("Float State missing with id " + STATE_FLOAT_ID, floatState);
		assertEquals(STATE_FLOAT_TIME_0, floatState.getStateAt(STATE_TIME_0).getValue().getValue());
		assertEquals(STATE_FLOAT_TIME_10, floatState.getStateAt(STATE_TIME_10).getValue().getValue());
		assertEquals(STATE_FLOAT_TIME_20, floatState.getStateAt(STATE_TIME_20).getValue().getValue());
		assertEquals(STATE_FLOAT_TIME_30, floatState.getStateAt(STATE_TIME_30).getValue().getValue());

		IntegerTimedState integerState = resource.getTimedState(STATE_INTEGER_ID);
		assertNotNull("Integer State missing with id " + STATE_INTEGER_ID, integerState);
		assertEquals(STATE_INTEGER_TIME_0, integerState.getStateAt(STATE_TIME_0).getValue().getValue());
		assertEquals(STATE_INTEGER_TIME_10, integerState.getStateAt(STATE_TIME_10).getValue().getValue());
		assertEquals(STATE_INTEGER_TIME_20, integerState.getStateAt(STATE_TIME_20).getValue().getValue());
		assertEquals(STATE_INTEGER_TIME_30, integerState.getStateAt(STATE_TIME_30).getValue().getValue());

		StringSetTimedState stringState = resource.getTimedState(STATE_STRING_ID);
		assertNotNull("String State missing with id " + STATE_STRING_ID, stringState);
		assertEquals(STATE_STRING_TIME_0, stringState.getStateAt(STATE_TIME_0).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_10, stringState.getStateAt(STATE_TIME_10).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_20, stringState.getStateAt(STATE_TIME_20).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_30, stringState.getStateAt(STATE_TIME_30).getValue().getValueAsString());
	}
}
