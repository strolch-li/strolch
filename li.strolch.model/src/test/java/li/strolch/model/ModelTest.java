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

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static li.strolch.model.ModelGenerator.*;
import static li.strolch.model.StrolchModelConstants.BAG_PARAMETERS;
import static li.strolch.model.Tags.*;
import static org.junit.Assert.*;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.parameter.*;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.ValueChange;
import li.strolch.model.visitor.StrolchElementDeepEqualsVisitor;
import li.strolch.utils.time.PeriodDuration;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("nls")
public class ModelTest {

	protected static final Logger logger = LoggerFactory.getLogger(ModelTest.class);

	@Test
	public void shouldCreateResource() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		assertEquals("@res01", resource.getId());
		assertEquals("Test resource", resource.getName());
		assertEquals("MyType", resource.getType());

		assertParams(resource);

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

		assertParams(order);
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

		assertParams(activity);

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

		assertParams(action);
		bag = action.getParameterBag(BAG_ID);
		validateBag(bag);

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
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcActivity);
		List<Locator> mismatches = dstActivity.accept(visitor);
		assertTrue("Same Activity should be deep equal!", mismatches.isEmpty());
	}

	@Test
	public void shouldPerformActivityClone() {
		Activity srcActivity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		Activity dstActivity = srcActivity.getClone();
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcActivity);
		List<Locator> mismatches = dstActivity.accept(visitor);
		assertTrue("Cloned Activity should be deep equal: " + mismatches, mismatches.isEmpty());
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
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcActivity);
		List<Locator> mismatches = dstActivity.accept(visitor);
		assertFalse("Activity should not be same if something has been changed", mismatches.isEmpty());
		assertEquals("Multiple changes should be registered", 6, mismatches.size());
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

		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcActivity);
		List<Locator> mismatches = dstActivity.accept(visitor);
		assertFalse("Activity should not be same if something has been changed", mismatches.isEmpty());
		assertEquals("Multiple changes should be registered", 9, mismatches.size());
	}

	@Test
	public void shouldPerformDeepResourceEquals() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcRes);
		List<Locator> mismatches = dstRes.accept(visitor);
		assertTrue("Same Resource should be deep equal!", mismatches.isEmpty());
	}

	@Test
	public void shouldPerformResourceClone() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = srcRes.getClone();
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcRes);
		List<Locator> mismatches = dstRes.accept(visitor);
		assertTrue("Cloned Resource should be deep equal!", mismatches.isEmpty());
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
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcRes);
		List<Locator> mismatches = dstRes.accept(visitor);
		assertFalse("Resource should not be same if param is changed!", mismatches.isEmpty());
		assertEquals("Multiple changes should be registered", 3, mismatches.size());
	}

	@Test
	public void shouldFailDeepResourceEquals2() {
		Resource srcRes = createResource("@res01", "Test resource", "MyType");
		Resource dstRes = createResource("@res01", "Test resource", "MyType");
		BooleanTimedState timedState = dstRes.getTimedState(STATE_BOOLEAN_ID);
		timedState.applyChange(new ValueChange<>(System.currentTimeMillis(), new BooleanValue(Boolean.FALSE)), true);
		timedState.setName("Ohla");
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcRes);
		List<Locator> mismatches = dstRes.accept(visitor);
		assertFalse("Resource should not be same if param is changed!", visitor.isEqual());
		assertEquals("Multiple change should be registered!", 2, mismatches.size());
	}

	@Test
	public void shouldPerformDeepOrderEquals() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		Order dstOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcOrder);
		List<Locator> mismatches = dstOrder.accept(visitor);
		assertTrue("Same Order should be deep equal: " + mismatches, visitor.isEqual());
	}

	@Test
	public void shouldPerformOrderClone() {
		Date date = new Date();
		Order srcOrder = createOrder("@ord01", "Test Order", "MyType", date, State.CREATED);
		Order dstOrder = srcOrder.getClone();
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcOrder);
		List<Locator> mismatches = dstOrder.accept(visitor);
		assertTrue("Cloned Order should be deep equal: " + mismatches, visitor.isEqual());
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
		StrolchElementDeepEqualsVisitor visitor = new StrolchElementDeepEqualsVisitor(srcOrder);
		List<Locator> mismatches = dstOrder.accept(visitor);
		assertFalse("Order should not be same if something has been changed", visitor.isEqual());
		assertEquals("Multiple changes should be registered", 5, mismatches.size());
	}

	public static void validateBag(ParameterBag bag) {

		assertNotNull(bag);

		assertEquals(BAG_ID, bag.getId());
		assertEquals(BAG_NAME, bag.getName());
		assertEquals(BAG_TYPE, bag.getType());

		validateParams(bag);
	}

	private void assertParams(ParameterBagContainer bagContainer) {
		Boolean bool = bagContainer.getParameter(BAG_ID, PARAM_BOOLEAN_ID).getValue();
		assertEquals(true, bool);

		String string = bagContainer.getParameter(BAG_ID, PARAM_STRING_ID).getValue();
		assertEquals("Strolch", string);

		Integer integer = bagContainer.getParameter(BAG_ID, PARAM_INTEGER_ID).getValue();
		assertEquals(77, integer.intValue());

		Date date = bagContainer.getParameter(BAG_ID, PARAM_DATE_ID).getValue();
		assertEquals(1354295525628L, date.getTime());

		Double doubl = bagContainer.getParameter(BAG_ID, PARAM_FLOAT_ID).getValue();
		assertEquals(44.3, doubl, 0.0001);

		Long lon = bagContainer.getParameter(BAG_ID, PARAM_LONG_ID).getValue();
		assertEquals(4453234566L, lon.longValue());

		List<Double> doubles = bagContainer.getParameter(BAG_ID, PARAM_LIST_FLOAT_ID).getValue();
		ArrayList<Double> floatList = new ArrayList<>();
		floatList.add(6.0);
		floatList.add(11.0);
		floatList.add(16.0);
		assertEquals(floatList, doubles);

		List<Integer> integers = bagContainer.getParameter(BAG_ID, PARAM_LIST_INTEGER_ID).getValue();
		ArrayList<Integer> intList = new ArrayList<>();
		intList.add(5);
		intList.add(10);
		intList.add(15);
		assertEquals(intList, integers);

		List<Long> longs = bagContainer.getParameter(BAG_ID, PARAM_LIST_LONG_ID).getValue();
		ArrayList<Long> longList = new ArrayList<>();
		longList.add(7L);
		longList.add(12L);
		longList.add(17L);
		assertEquals(longList, longs);

		List<String> strings = bagContainer.getParameter(BAG_ID, PARAM_LIST_STRING_ID).getValue();
		ArrayList<String> stringList = new ArrayList<>();
		stringList.add("Hello");
		stringList.add("World");
		assertEquals(stringList, strings);
	}

	public static void validateParams(ParameterBag bag) {

		BooleanParameter boolParam = bag.getParameter(PARAM_BOOLEAN_ID);
		assertNotNull("Boolean Param missing with id " + PARAM_BOOLEAN_ID, boolParam);
		assertEquals(true, boolParam.getValue());

		FloatParameter floatParam = bag.getParameter(PARAM_FLOAT_ID);
		assertNotNull("Float Param missing with id " + PARAM_FLOAT_ID, floatParam);
		assertEquals(44.3, floatParam.getValue(), 0.0001);

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
		stringListP.clear();
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
		intListP.clear();
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
		floatListP.clear();
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
		longListP.clear();
		assertEquals("", longListP.getValueAsString());
		longListP.addValue(55L);
		assertEquals("55", longListP.getValueAsString());
	}

	private void validateStates(Resource resource) {
		BooleanTimedState booleanState = resource.getTimedState(STATE_BOOLEAN_ID);
		assertNotNull("Boolean State missing with id " + STATE_BOOLEAN_ID, booleanState);
		assertEquals(STATE_BOOLEAN_TIME_0, booleanState.getStateAt(STATE_TIME_0).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_10, booleanState.getStateAt(STATE_TIME_10).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_20, booleanState.getStateAt(STATE_TIME_20).getValue().getValue());
		assertEquals(STATE_BOOLEAN_TIME_30, booleanState.getStateAt(STATE_TIME_30).getValue().getValue());

		FloatTimedState floatState = resource.getTimedState(STATE_FLOAT_ID);
		assertNotNull("Float State missing with id " + STATE_FLOAT_ID, floatState);
		assertEquals(STATE_FLOAT_TIME_0, floatState.getStateAt(STATE_TIME_0).getValue().getValue(), 0.0);
		assertEquals(STATE_FLOAT_TIME_10, floatState.getStateAt(STATE_TIME_10).getValue().getValue(), 0.0);
		assertEquals(STATE_FLOAT_TIME_20, floatState.getStateAt(STATE_TIME_20).getValue().getValue(), 0.0);
		assertEquals(STATE_FLOAT_TIME_30, floatState.getStateAt(STATE_TIME_30).getValue().getValue(), 0.0);

		FloatListTimedState floatListState = resource.getTimedState(STATE_FLOAT_LIST_ID);
		assertNotNull("Float List State missing with id " + STATE_FLOAT_ID, floatListState);
		assertEquals(singletonList(STATE_FLOAT_TIME_0), floatListState.getStateAt(STATE_TIME_0).getValue().getValue());
		assertEquals(singletonList(STATE_FLOAT_TIME_0), floatListState.getStateAt(STATE_TIME_10).getValue().getValue());
		assertEquals(singletonList(STATE_FLOAT_TIME_0), floatListState.getStateAt(STATE_TIME_20).getValue().getValue());
		assertEquals(asList(STATE_FLOAT_TIME_0, STATE_FLOAT_TIME_10, STATE_FLOAT_TIME_20),
				floatListState.getStateAt(STATE_TIME_30).getValue().getValue());

		IntegerTimedState integerState = resource.getTimedState(STATE_INTEGER_ID);
		assertNotNull("Integer State missing with id " + STATE_INTEGER_ID, integerState);
		assertEquals(STATE_INTEGER_TIME_0, integerState.getStateAt(STATE_TIME_0).getValue().getValue(), 0.0);
		assertEquals(STATE_INTEGER_TIME_10, integerState.getStateAt(STATE_TIME_10).getValue().getValue(), 0.0);
		assertEquals(STATE_INTEGER_TIME_20, integerState.getStateAt(STATE_TIME_20).getValue().getValue(), 0.0);
		assertEquals(STATE_INTEGER_TIME_30, integerState.getStateAt(STATE_TIME_30).getValue().getValue(), 0.0);

		StringSetTimedState stringState = resource.getTimedState(STATE_STRING_ID);
		assertNotNull("String State missing with id " + STATE_STRING_ID, stringState);
		assertEquals(STATE_STRING_TIME_0, stringState.getStateAt(STATE_TIME_0).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_10, stringState.getStateAt(STATE_TIME_10).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_20, stringState.getStateAt(STATE_TIME_20).getValue().getValueAsString());
		assertEquals(STATE_STRING_TIME_30, stringState.getStateAt(STATE_TIME_30).getValue().getValueAsString());
	}

	@Test
	public void shouldGetSetValues() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		resource.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertGetSetParameterValues(resource);

		Order order = createOrder("@ord01", "Test Order", "MyType");
		order.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertGetSetParameterValues(order);

		Activity activity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		activity.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertGetSetParameterValues(activity);

		Action action = createAction("action", "Action", "Use");
		action.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertGetSetParameterValues(action);
	}

	@Test
	public void shouldGetParams() {

		Resource resource = createResource("@res01", "Test resource", "MyType");
		resource.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertParameters(resource);

		Order order = createOrder("@ord01", "Test Order", "MyType");
		order.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertParameters(order);

		Activity activity = createActivity("@act01", "Test Activity", "MyType", TimeOrdering.SERIES);
		activity.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertParameters(activity);

		Action action = createAction("action", "Action", "Use");
		action.addParameterBag(createParameterBag(BAG_PARAMETERS, "Parameters", "Parameters"));
		assertParameters(action);
	}

	private void assertParameters(GroupedParameterizedElement e) {

		// default "parameters" bag
		assertEquals(StrolchValueType.BOOLEAN, e.getBooleanP(PARAM_BOOLEAN_ID).getValueType());
		assertEquals(StrolchValueType.INTEGER, e.getIntegerP(PARAM_INTEGER_ID).getValueType());
		assertEquals(StrolchValueType.FLOAT, e.getDoubleP(PARAM_FLOAT_ID).getValueType());
		assertEquals(StrolchValueType.STRING, e.getStringP(PARAM_STRING_ID).getValueType());
		assertEquals(StrolchValueType.LONG, e.getLongP(PARAM_LONG_ID).getValueType());
		assertEquals(StrolchValueType.TEXT, e.getTextP(PARAM_TEXT_ID).getValueType());
		assertEquals(StrolchValueType.DATE, e.getDateP(PARAM_DATE_ID).getValueType());
		assertEquals(StrolchValueType.DURATION, e.getDurationP(PARAM_DURATION_ID).getValueType());
		assertEquals(StrolchValueType.STRING_LIST, e.getStringListP(PARAM_LIST_STRING_ID).getValueType());
		assertEquals(StrolchValueType.INTEGER_LIST, e.getIntegerListP(PARAM_LIST_INTEGER_ID).getValueType());
		assertEquals(StrolchValueType.LONG_LIST, e.getLongListP(PARAM_LIST_LONG_ID).getValueType());
		assertEquals(StrolchValueType.FLOAT_LIST, e.getDoubleListP(PARAM_LIST_FLOAT_ID).getValueType());

		// explicit bag
		assertEquals(StrolchValueType.BOOLEAN, e.getBooleanP(BAG_ID, PARAM_BOOLEAN_ID).getValueType());
		assertEquals(StrolchValueType.INTEGER, e.getIntegerP(BAG_ID, PARAM_INTEGER_ID).getValueType());
		assertEquals(StrolchValueType.FLOAT, e.getDoubleP(BAG_ID, PARAM_FLOAT_ID).getValueType());
		assertEquals(StrolchValueType.STRING, e.getStringP(BAG_ID, PARAM_STRING_ID).getValueType());
		assertEquals(StrolchValueType.LONG, e.getLongP(BAG_ID, PARAM_LONG_ID).getValueType());
		assertEquals(StrolchValueType.TEXT, e.getTextP(BAG_ID, PARAM_TEXT_ID).getValueType());
		assertEquals(StrolchValueType.DATE, e.getDateP(BAG_ID, PARAM_DATE_ID).getValueType());
		assertEquals(StrolchValueType.DURATION, e.getDurationP(BAG_ID, PARAM_DURATION_ID).getValueType());
		assertEquals(StrolchValueType.STRING_LIST, e.getStringListP(BAG_ID, PARAM_LIST_STRING_ID).getValueType());
		assertEquals(StrolchValueType.INTEGER_LIST, e.getIntegerListP(BAG_ID, PARAM_LIST_INTEGER_ID).getValueType());
		assertEquals(StrolchValueType.LONG_LIST, e.getLongListP(BAG_ID, PARAM_LIST_LONG_ID).getValueType());
		assertEquals(StrolchValueType.FLOAT_LIST, e.getDoubleListP(BAG_ID, PARAM_LIST_FLOAT_ID).getValueType());
	}

	private void assertGetSetParameterValues(GroupedParameterizedElement e) {

		/*
		 * default "parameters" bag
		 */

		// boolean param
		assertTrue(e.getBoolean(PARAM_BOOLEAN_ID));
		e.setBoolean(PARAM_BOOLEAN_ID, false);
		assertFalse(e.getBoolean(PARAM_BOOLEAN_ID));

		// integer param
		assertEquals(77, e.getInteger(PARAM_INTEGER_ID));
		e.setInteger(PARAM_INTEGER_ID, 88);
		assertEquals(88, e.getInteger(PARAM_INTEGER_ID));

		// float param
		assertEquals(44.3, e.getDouble(PARAM_FLOAT_ID), 0.0);
		e.setDouble(PARAM_FLOAT_ID, 56.0);
		assertEquals(56.0, e.getDouble(PARAM_FLOAT_ID), 0.0);

		// string param
		assertEquals("Strolch", e.getString(PARAM_STRING_ID));
		e.setString(PARAM_STRING_ID, "aa");
		assertEquals("aa", e.getString(PARAM_STRING_ID));

		// long param
		assertEquals(4453234566L, e.getLong(PARAM_LONG_ID));
		e.setLong(PARAM_LONG_ID, 0L);
		assertEquals(0L, e.getLong(PARAM_LONG_ID));

		// text param
		assertEquals("Strolch\n\nmulti\n\n\nline", e.getText(PARAM_TEXT_ID));
		e.setText(PARAM_TEXT_ID, "bla");
		assertEquals("bla", e.getText(PARAM_TEXT_ID));

		// date param
		assertEquals(ZonedDateTime.ofInstant(new Date(1354295525628L).toInstant(), ZoneId.systemDefault()),
				e.getDate(PARAM_DATE_ID));
		ZonedDateTime now = ZonedDateTime.now();
		e.setDate(PARAM_DATE_ID, now);
		assertEquals(now, e.getDate(PARAM_DATE_ID));

		// duration param
		assertEquals(PeriodDuration.parse("P1D"), e.getDuration(PARAM_DURATION_ID));
		e.setDuration(PARAM_DURATION_ID, PeriodDuration.parse("PT1H"));
		assertEquals(PeriodDuration.parse("PT1H"), e.getDuration(PARAM_DURATION_ID));

		// string list param
		assertEquals(asList("Hello", "World"), e.getStringList(PARAM_LIST_STRING_ID));
		e.setStringList(PARAM_LIST_STRING_ID, asList("a", "b"));
		assertEquals(asList("a", "b"), e.getStringList(PARAM_LIST_STRING_ID));

		// integer list param
		assertEquals(asList(5, 10, 15), e.getIntegerList(PARAM_LIST_INTEGER_ID));
		e.setIntegerList(PARAM_LIST_INTEGER_ID, asList(6, 2));
		assertEquals(asList(6, 2), e.getIntegerList(PARAM_LIST_INTEGER_ID));

		// long list param
		assertEquals(asList(7L, 12L, 17L), e.getLongList(PARAM_LIST_LONG_ID));
		e.setLongList(PARAM_LIST_LONG_ID, asList(6L, 2L));
		assertEquals(asList(6L, 2L), e.getLongList(PARAM_LIST_LONG_ID));

		// float list param
		assertEquals(asList(6.0, 11.0, 16.0), e.getDoubleList(PARAM_LIST_FLOAT_ID));
		e.setDoubleList(PARAM_LIST_FLOAT_ID, asList(6.0, 2.0));
		assertEquals(asList(6.0, 2.0), e.getDoubleList(PARAM_LIST_FLOAT_ID));


		/*
		 * explicit bag
		 */

		// boolean param
		assertTrue(e.getBoolean(BAG_ID, PARAM_BOOLEAN_ID));
		e.setBoolean(BAG_ID, PARAM_BOOLEAN_ID, false);
		assertFalse(e.getBoolean(BAG_ID, PARAM_BOOLEAN_ID));

		// integer param
		assertEquals(77, e.getInteger(BAG_ID, PARAM_INTEGER_ID));
		e.setInteger(BAG_ID, PARAM_INTEGER_ID, 188);
		assertEquals(188, e.getInteger(BAG_ID, PARAM_INTEGER_ID));

		// float param
		assertEquals(44.3, e.getDouble(BAG_ID, PARAM_FLOAT_ID), 0.0);
		e.setDouble(BAG_ID, PARAM_FLOAT_ID, 156.0);
		assertEquals(156.0, e.getDouble(BAG_ID, PARAM_FLOAT_ID), 0.0);

		// string param
		assertEquals("Strolch", e.getString(BAG_ID, PARAM_STRING_ID));
		e.setString(BAG_ID, PARAM_STRING_ID, "aaa");
		assertEquals("aaa", e.getString(BAG_ID, PARAM_STRING_ID));

		// long param
		assertEquals(4453234566L, e.getLong(BAG_ID, PARAM_LONG_ID));
		e.setLong(BAG_ID, PARAM_LONG_ID, 1L);
		assertEquals(1L, e.getLong(BAG_ID, PARAM_LONG_ID));

		// text param
		assertEquals("Strolch\n\nmulti\n\n\nline", e.getText(BAG_ID, PARAM_TEXT_ID));
		e.setText(BAG_ID, PARAM_TEXT_ID, "bla bla");
		assertEquals("bla bla", e.getText(BAG_ID, PARAM_TEXT_ID));

		// date param
		assertEquals(ZonedDateTime.ofInstant(new Date(1354295525628L).toInstant(), ZoneId.systemDefault()),
				e.getDate(BAG_ID, PARAM_DATE_ID));
		now = ZonedDateTime.now().plusDays(1);
		e.setDate(BAG_ID, PARAM_DATE_ID, now);
		assertEquals(now, e.getDate(BAG_ID, PARAM_DATE_ID));

		// duration param
		assertEquals(PeriodDuration.parse("P1D"), e.getDuration(BAG_ID, PARAM_DURATION_ID));
		e.setDuration(BAG_ID, PARAM_DURATION_ID, PeriodDuration.parse("PT2H"));
		assertEquals(PeriodDuration.parse("PT2H"), e.getDuration(BAG_ID, PARAM_DURATION_ID));

		// string list param
		assertEquals(asList("Hello", "World"), e.getStringList(BAG_ID, PARAM_LIST_STRING_ID));
		e.setStringList(BAG_ID, PARAM_LIST_STRING_ID, asList("aa", "b"));
		assertEquals(asList("aa", "b"), e.getStringList(BAG_ID, PARAM_LIST_STRING_ID));

		// integer list param
		assertEquals(asList(5, 10, 15), e.getIntegerList(BAG_ID, PARAM_LIST_INTEGER_ID));
		e.setIntegerList(BAG_ID, PARAM_LIST_INTEGER_ID, asList(76, 2));
		assertEquals(asList(76, 2), e.getIntegerList(BAG_ID, PARAM_LIST_INTEGER_ID));

		// long list param
		assertEquals(asList(7L, 12L, 17L), e.getLongList(BAG_ID, PARAM_LIST_LONG_ID));
		e.setLongList(BAG_ID, PARAM_LIST_LONG_ID, asList(76L, 2L));
		assertEquals(asList(76L, 2L), e.getLongList(BAG_ID, PARAM_LIST_LONG_ID));

		// float list param
		assertEquals(asList(6.0, 11.0, 16.0), e.getDoubleList(BAG_ID, PARAM_LIST_FLOAT_ID));
		e.setDoubleList(BAG_ID, PARAM_LIST_FLOAT_ID, asList(76.0, 2.0));
		assertEquals(asList(76.0, 2.0), e.getDoubleList(BAG_ID, PARAM_LIST_FLOAT_ID));
	}
}
