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

import static li.strolch.model.ModelGenerator.ACTION_RES_ID;
import static li.strolch.model.ModelGenerator.ACTION_RES_TYPE;
import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.BAG_NAME;
import static li.strolch.model.ModelGenerator.BAG_TYPE;
import static li.strolch.model.ModelGenerator.PARAM_BOOLEAN_ID;
import static li.strolch.model.ModelGenerator.PARAM_DATE_ID;
import static li.strolch.model.ModelGenerator.PARAM_FLOAT_ID;
import static li.strolch.model.ModelGenerator.PARAM_INTEGER_ID;
import static li.strolch.model.ModelGenerator.PARAM_LIST_FLOAT_ID;
import static li.strolch.model.ModelGenerator.PARAM_LIST_INTEGER_ID;
import static li.strolch.model.ModelGenerator.PARAM_LIST_LONG_ID;
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
import static li.strolch.model.ModelGenerator.createActivity;
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
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatListParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerListParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongListParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.model.visitor.ActivityDeepEqualsVisitor;
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;

@SuppressWarnings("nls")
public class ModelTest {

	protected static final Logger logger = LoggerFactory.getLogger(ModelTest.class);

	@Test
	public void shouldCreateResource() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		assertEquals("@res01", resource.getId());
		assertEquals("Test resource", resource.getName());
		assertEquals("MyType", resource.getType());

		ParameterBag bag = resource.getParameterBag(BAG_ID);
		validateBag(bag);

		validateStates(resource);
	}

	@Test
	public void shouldCreateOrder() {

		Date date = new Date();
		Order order = createOrder("@ord01", "Test Order", "MyType", date, State.CLOSED);
		assertEquals("@ord01", order.getId());
		assertEquals("Test Order", order.getName());
		assertEquals("MyType", order.getType());
		assertEquals(date, order.getDate());
		assertEquals(State.CLOSED, order.getState());

		ParameterBag bag = order.getParameterBag(BAG_ID);
		validateBag(bag);
	}

	@Test
	public void shouldCreateActivity() {

		String actId = "@act01";
		String actName = "Test Activity";
		String actType = "MyType";

		List<IValueChange<? extends IValue<?>>> changes = new ArrayList<>();
		changes.add(new ValueChange<>(0L, new IntegerValue(0), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(10L, new IntegerValue(10), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(20L, new IntegerValue(20), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(30L, new IntegerValue(30), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(40L, new IntegerValue(20), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(50L, new IntegerValue(10), STATE_INTEGER_ID));
		changes.add(new ValueChange<>(60L, new IntegerValue(0), STATE_INTEGER_ID));

		Activity activity = createActivity(actId, actName, actType, TimeOrdering.SERIES);
		assertEquals(actId, activity.getId());
		assertEquals(actName, activity.getName());
		assertEquals(actType, activity.getType());

		ParameterBag bag = activity.getParameterBag(BAG_ID);
		validateBag(bag);

		Action action = activity.getElement("action_" + actId);
		action.setState(State.ERROR);
		assertEquals("action_" + actId, action.getId());
		assertEquals("Action " + actName, action.getName());
		assertEquals("Use", action.getType());
		assertEquals(ACTION_RES_ID, action.getResourceId());
		assertEquals(ACTION_RES_TYPE, action.getResourceType());
		assertEquals(changes, action.getChanges());

		activity = activity.getElement("sub_" + actId);
		assertEquals("sub_" + actId, activity.getId());
		assertEquals("sub_" + actName, activity.getName());
		assertEquals(actType, activity.getType());
		bag = activity.getParameterBag(BAG_ID);
		validateBag(bag);

		action = activity.getElement("action_" + actId);
		action.setState(State.ERROR);
		assertEquals("action_" + actId, action.getId());
		assertEquals("Action " + actName, action.getName());
		assertEquals("Use", action.getType());
		assertEquals(ACTION_RES_ID, action.getResourceId());
		assertEquals(ACTION_RES_TYPE, action.getResourceType());
		assertEquals(changes, action.getChanges());

		activity = activity.getElement("subSub_" + actId);
		assertEquals("subSub_" + actId, activity.getId());
		assertEquals("subSub_" + actName, activity.getName());
		assertEquals(actType, activity.getType());
		bag = activity.getParameterBag(BAG_ID);
		validateBag(bag);

		action = activity.getElement("action1_" + actId);
		action.setState(State.EXECUTION);
		assertEquals("action1_" + actId, action.getId());
		assertEquals("Action " + actName, action.getName());
		assertEquals("Use", action.getType());
		assertEquals(ACTION_RES_ID, action.getResourceId());
		assertEquals(ACTION_RES_TYPE, action.getResourceType());
		assertEquals(changes, action.getChanges());

		List<Action> actions = activity.getRootElement().getActionsWithState(State.ERROR);
		assertEquals(2, actions.size());

		actions = activity.getRootElement().getActionsWithState(State.EXECUTION);
		assertEquals(1, actions.size());

		actions = activity.getRootElement().getActionsWithState(State.CREATED);
		assertEquals(1, actions.size());
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

		Order order = createOrder("@ord01", "Test Order", "MyType", new Date(), State.CLOSED);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01"), order.getLocator());
		bag = order.getParameterBag(BAG_ID);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01", BAG, BAG_ID), bag.getLocator());
		sP = bag.getParameter(PARAM_STRING_ID);
		assertEquals(Locator.valueOf(ORDER, "MyType", "@ord01", BAG, BAG_ID, PARAM_STRING_ID), sP.getLocator());
	}

	@Test
	public void shouldPerformDeepActivityEquals() {
		Activity srcActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		Activity dstActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(srcActivity);
		visitor.visit(dstActivity);
		assertTrue("Same Activity should be deep equal!", visitor.isEqual());
	}

	@Test
	public void shouldPerformActivityClone() {
		Activity srcActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		Activity dstActivity = srcActivity.getClone();
		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(srcActivity);
		visitor.visit(dstActivity);
		assertTrue("Cloned Activity should be deep equal: " + visitor.getMismatchedLocators(), visitor.isEqual());
	}

	@Test
	public void shouldFailDeepActivityEquals1() {
		Activity srcActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		Activity dstActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		dstActivity.setName("Bla");
		dstActivity.setType("BlaBla");
		ParameterBag bag = dstActivity.getParameterBag(BAG_ID);
		bag.setName("Bla bla");
		FloatParameter fParam = bag.getParameter(PARAM_FLOAT_ID);
		fParam.setValue(23434234.234);
		fParam.setName("Ohla");
		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(srcActivity);
		visitor.visit(dstActivity);
		assertFalse("Activity should not be same if something has been changed", visitor.isEqual());
		assertEquals("Multiple changes should be registered", 6, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldFailDeepActivityEquals2() {
		Activity srcActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		Activity dstActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);

		Action action = dstActivity.getElement("action_" + "@act01");
		action.setResourceId("Bla");
		action.setResourceType("Bla");
		action.setType("Bla");
		action.setState(State.CLOSED);
		action.addChange(new ValueChange<>(1234567890L, new IntegerValue(12345), STATE_INTEGER_ID));

		Activity activity = dstActivity.getElement("sub_" + "@act01");
		activity.addElement(new Action("bla", "Bla", "Bla"));

		action = activity.getElement("action_" + "@act01");
		action.addChange(new ValueChange<>(1234567890L, new IntegerValue(12345), STATE_INTEGER_ID));

		activity = activity.getElement("subSub_" + "@act01");
		activity.addElement(new Action("bla", "Bla", "Bla"));

		action = activity.getElement("action1_" + "@act01");
		action.addChange(new ValueChange<>(1234567890L, new IntegerValue(12345), STATE_INTEGER_ID));

		ActivityDeepEqualsVisitor visitor = new ActivityDeepEqualsVisitor(srcActivity);
		visitor.visit(dstActivity);
		assertFalse("Activity should not be same if something has been changed", visitor.isEqual());
		assertEquals("Multiple changes should be registered", 9, visitor.getMismatchedLocators().size());
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
	public void shouldPerformResourceClone() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = srcRes.getClone();
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertTrue("Cloned Resource should be deep equal!", visitor.isEqual());
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
		assertEquals("Multiple changes should be registered", 3, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldFailDeepResourceEquals2() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		BooleanTimedState timedState = dstRes.getTimedState(STATE_BOOLEAN_ID);
		timedState.applyChange(new ValueChange<>(System.currentTimeMillis(), new BooleanValue(Boolean.FALSE)));
		timedState.setName("Ohla");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertFalse("Resource should not be same if param is changed!", visitor.isEqual());
		assertEquals("Multiple change should be registered!", 2, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldPerformDeepOrderEquals() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		Order dstOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		assertTrue("Same Order should be deep equal: " + visitor.getMismatchedLocators(), visitor.isEqual());
	}

	@Test
	public void shouldPerformOrderClone() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		Order dstOrder = srcOrder.getClone();
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		assertTrue("Cloned Order should be deep equal: " + visitor.getMismatchedLocators(), visitor.isEqual());
	}

	@Test
	public void shouldFailDeepOrderEquals1() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		Order dstOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		dstOrder.setDate(new Date(1L));
		dstOrder.setState(State.CLOSED);
		ParameterBag bag = dstOrder.getParameterBag(BAG_ID);
		bag.setName("Bla bla");
		FloatParameter fParam = bag.getParameter(PARAM_FLOAT_ID);
		fParam.setValue(23434234.234);
		fParam.setName("Ohla");
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		assertFalse("Order should not be same if something has been changed", visitor.isEqual());
		assertEquals("Multiple changes should be registered", 5, visitor.getMismatchedLocators().size());
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
		assertEquals("Hello, World", stringListP.getValueAsString());
		stringListP.setValueFromString("a;b");
		assertEquals("a, b", stringListP.getValueAsString());
		stringListP.setValueFromString("a,b");
		assertEquals("a, b", stringListP.getValueAsString());
		stringListP.clearValue();
		assertEquals("", stringListP.getValueAsString());
		stringListP.addValue("a");
		assertEquals("a", stringListP.getValueAsString());

		IntegerListParameter intListP = bag.getParameter(PARAM_LIST_INTEGER_ID);
		assertNotNull("IntegerList Param missing with id " + PARAM_LIST_INTEGER_ID, intListP);
		ArrayList<Integer> intList = new ArrayList<>();
		intList.add(5);
		intList.add(10);
		intList.add(15);
		assertEquals(intList, intListP.getValue());
		assertEquals("5, 10, 15", intListP.getValueAsString());
		intListP.setValueFromString("4;45");
		assertEquals("4, 45", intListP.getValueAsString());
		intListP.setValueFromString("4,45");
		assertEquals("4, 45", intListP.getValueAsString());
		intListP.clearValue();
		assertEquals("", intListP.getValueAsString());
		intListP.addValue(55);
		assertEquals("55", intListP.getValueAsString());

		FloatListParameter floatListP = bag.getParameter(PARAM_LIST_FLOAT_ID);
		assertNotNull("FloatList Param missing with id " + PARAM_LIST_FLOAT_ID, floatListP);
		ArrayList<Double> floatList = new ArrayList<>();
		floatList.add(6.0);
		floatList.add(11.0);
		floatList.add(16.0);
		assertEquals(floatList, floatListP.getValue());
		assertEquals("6.0, 11.0, 16.0", floatListP.getValueAsString());
		floatListP.setValueFromString("4.2;4.1");
		assertEquals("4.2, 4.1", floatListP.getValueAsString());
		floatListP.setValueFromString("4.2,4.1");
		assertEquals("4.2, 4.1", floatListP.getValueAsString());
		floatListP.clearValue();
		assertEquals("", floatListP.getValueAsString());
		floatListP.addValue(55.5);
		assertEquals("55.5", floatListP.getValueAsString());

		LongListParameter longListP = bag.getParameter(PARAM_LIST_LONG_ID);
		assertNotNull("LongList Param missing with id " + PARAM_LIST_LONG_ID, longListP);
		ArrayList<Long> longList = new ArrayList<>();
		longList.add(7L);
		longList.add(12L);
		longList.add(17L);
		assertEquals(longList, longListP.getValue());
		assertEquals("7, 12, 17", longListP.getValueAsString());
		longListP.setValueFromString("4;4");
		assertEquals("4, 4", longListP.getValueAsString());
		longListP.setValueFromString("4,4");
		assertEquals("4, 4", longListP.getValueAsString());
		longListP.clearValue();
		assertEquals("", longListP.getValueAsString());
		longListP.addValue(55L);
		assertEquals("55", longListP.getValueAsString());
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
