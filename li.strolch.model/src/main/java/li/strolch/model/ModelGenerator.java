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

import java.util.*;

import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.TimeOrdering;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.parameter.*;
import li.strolch.model.policy.JavaPolicyDef;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.impl.*;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * Class which can be used to generate objects which implement {@link StrolchElement}. These generated classes can then
 * be used in test classes etc.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class ModelGenerator {

	public static final String PARAM_BOOLEAN_ID = "@param1";
	public static final String PARAM_BOOLEAN_NAME = "Boolean Param";

	public static final String PARAM_FLOAT_ID = "@param2";
	public static final String PARAM_FLOAT_NAME = "Float Param";

	public static final String PARAM_INTEGER_ID = "@param3";
	public static final String PARAM_INTEGER_NAME = "Integer Param";

	public static final String PARAM_LONG_ID = "@param4";
	public static final String PARAM_LONG_NAME = "Long Param";

	public static final String PARAM_STRING_ID = "@param5";
	public static final String PARAM_STRING_NAME = "String Param";

	public static final String PARAM_DATE_ID = "@param6";
	public static final String PARAM_DATE_NAME = "Date Param";

	public static final String PARAM_LIST_STRING_ID = "@param7";
	public static final String PARAM_LIST_STRING_NAME = "StringList Param";

	public static final String PARAM_DURATION_ID = "@param8";
	public static final String PARAM_DURATION_NAME = "Duration Param";

	public static final String PARAM_LIST_INTEGER_ID = "@param9";
	public static final String PARAM_LIST_INTEGER_NAME = "IntegerList Param";

	public static final String PARAM_LIST_FLOAT_ID = "@param10";
	public static final String PARAM_LIST_FLOAT_NAME = "FloatList Param";

	public static final String PARAM_LIST_LONG_ID = "@param11";
	public static final String PARAM_LIST_LONG_NAME = "LongList Param";

	public static final String STATE_FLOAT_ID = "@state1";
	public static final String STATE_FLOAT_NAME = "Float State";

	public static final String STATE_INTEGER_ID = "@state2";
	public static final String STATE_INTEGER_NAME = "Integer State";

	public static final String STATE_STRING_ID = "@state3";
	public static final String STATE_STRING_NAME = "String State";

	public static final String STATE_BOOLEAN_ID = "@state4";
	public static final String STATE_BOOLEAN_NAME = "Boolean State";

	public static final long STATE_TIME_0 = 0L;
	public static final long STATE_TIME_10 = 10L;
	public static final long STATE_TIME_20 = 20L;
	public static final long STATE_TIME_30 = 30L;
	public static final long STATE_TIME_40 = 40L;

	public static final Double STATE_FLOAT_TIME_0 = 0.0D;
	public static final Double STATE_FLOAT_TIME_10 = 10.0D;
	public static final Double STATE_FLOAT_TIME_20 = 20.0D;
	public static final Double STATE_FLOAT_TIME_30 = 30.0D;

	public static final Integer STATE_INTEGER_TIME_0 = 0;
	public static final Integer STATE_INTEGER_TIME_10 = 10;
	public static final Integer STATE_INTEGER_TIME_20 = 20;
	public static final Integer STATE_INTEGER_TIME_30 = 30;

	public static final String STATE_STRING_TIME_0 = "empty";
	public static final String STATE_STRING_TIME_10 = "a";
	public static final String STATE_STRING_TIME_20 = "b";
	public static final String STATE_STRING_TIME_30 = "c";

	public static final Boolean STATE_BOOLEAN_TIME_0 = Boolean.FALSE;
	public static final Boolean STATE_BOOLEAN_TIME_10 = Boolean.TRUE;
	public static final Boolean STATE_BOOLEAN_TIME_20 = Boolean.FALSE;
	public static final Boolean STATE_BOOLEAN_TIME_30 = Boolean.TRUE;

	public static final String BAG_ID = "@bag01";
	public static final String BAG_NAME = "Test Bag";
	public static final String BAG_TYPE = "TestBag";

	public static final String ACTION_RES_TYPE = "ResType";
	public static final String ACTION_RES_ID = "@resId";

	/**
	 * Creates an {@link Resource} with the given values and adds a {@link ParameterBag} by calling {@link
	 * #createParameterBag(String, String, String)}
	 *
	 * @param id
	 * 		the id of the {@link Resource}
	 * @param name
	 * 		the name of the {@link Resource}
	 * @param type
	 * 		the type of the {@link Resource}
	 *
	 * @return the newly created {@link Resource}
	 */
	public static Resource createResource(String id, String name, String type) {
		Resource resource = new Resource(id, name, type);
		ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		resource.addParameterBag(bag);
		addTimedStates(resource);

		resource.setPolicyDefs(createPolicyDefs());

		return resource;
	}

	/**
	 * Creates {@link StrolchTimedState} instances and adds them to the {@link Resource}
	 *
	 * @param resource
	 * 		the resource to which to addd the newly created {@link StrolchTimedState}
	 */
	public static void addTimedStates(Resource resource) {

		// float state
		FloatTimedState floatTimedState = new FloatTimedState(STATE_FLOAT_ID, STATE_FLOAT_NAME);
		floatTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new FloatValue(STATE_FLOAT_TIME_0)), true);
		FloatValue floatValueChange = new FloatValue(STATE_FLOAT_TIME_10);
		floatTimedState.applyChange(new ValueChange<>(STATE_TIME_10, floatValueChange), true);
		floatTimedState.applyChange(new ValueChange<>(STATE_TIME_20, floatValueChange), true);
		floatTimedState.applyChange(new ValueChange<>(STATE_TIME_30, floatValueChange), true);
		resource.addTimedState(floatTimedState);

		// integer state
		IntegerTimedState integerTimedState = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
		integerTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new IntegerValue(STATE_INTEGER_TIME_0)), true);
		IntegerValue integerValueChange = new IntegerValue(STATE_INTEGER_TIME_10);
		integerTimedState.applyChange(new ValueChange<>(STATE_TIME_10, integerValueChange), true);
		integerTimedState.applyChange(new ValueChange<>(STATE_TIME_20, integerValueChange), true);
		integerTimedState.applyChange(new ValueChange<>(STATE_TIME_30, integerValueChange), true);
		resource.addTimedState(integerTimedState);

		// boolean state
		BooleanTimedState booleanTimedState = new BooleanTimedState(STATE_BOOLEAN_ID, STATE_BOOLEAN_NAME);
		booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new BooleanValue(STATE_BOOLEAN_TIME_0)), true);
		BooleanValue booleanValueChange = new BooleanValue(STATE_BOOLEAN_TIME_10);
		booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_10, booleanValueChange), true);
		booleanValueChange = booleanValueChange.getInverse();
		booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_20, booleanValueChange), true);
		booleanValueChange = booleanValueChange.getInverse();
		booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_30, booleanValueChange), true);
		resource.addTimedState(booleanTimedState);

		// string state
		StringSetTimedState stringTimedState = new StringSetTimedState(STATE_STRING_ID, STATE_STRING_NAME);
		StringSetValue change = new StringSetValue(asSet(STATE_STRING_TIME_0));
		stringTimedState.applyChange(new ValueChange<>(STATE_TIME_0, change), true);
		change = change.getInverse();
		change.add(asSet(STATE_STRING_TIME_10));
		stringTimedState.applyChange(new ValueChange<>(STATE_TIME_10, change), true);
		removeInverted(change.getValue());
		change = change.getInverse();
		change.add(asSet(STATE_STRING_TIME_20));
		stringTimedState.applyChange(new ValueChange<>(STATE_TIME_20, change), true);
		removeInverted(change.getValue());
		change = change.getInverse();
		change.add(asSet(STATE_STRING_TIME_30));
		stringTimedState.applyChange(new ValueChange<>(STATE_TIME_30, change), true);
		resource.addTimedState(stringTimedState);
	}

	private static Set<AString> asSet(String value) {
		HashSet<AString> hashSet = new HashSet<>();
		hashSet.add(new AString(value));
		return hashSet;
	}

	private static void removeInverted(Set<AString> set) {
		set.removeIf(AString::isInverse);
	}

	/**
	 * Creates a list of {@link Resource Resources} with the given values and adds a {@link ParameterBag} by calling
	 * {@link #createParameterBag(String, String, String)}
	 *
	 * @param idStart
	 * 		id range start
	 * @param count
	 * 		the number of elements to create
	 * @param idPrefix
	 * 		the prefix to generate IDs for the {@link Resource Resources}
	 * @param name
	 * 		the name of the {@link Resource}
	 * @param type
	 * 		the type of the {@link Resource}
	 *
	 * @return the list of newly created {@link Resource Resources}
	 */
	public static List<Resource> createResources(int idStart, int count, String idPrefix, String name, String type) {
		List<Resource> resources = new ArrayList<>();
		for (int i = 0; i < count; i++) {
			String id = StringHelper.normalizeLength(String.valueOf((i + idStart)), 8, true, '0');
			resources.add(createResource(idPrefix + id, name + " " + i, type));
		}
		return resources;
	}

	/**
	 * Creates an {@link Order} with the given values and adds a {@link ParameterBag} by calling {@link
	 * #createParameterBag(String, String, String)}
	 *
	 * @param id
	 * 		the id of the {@link Order}
	 * @param name
	 * 		the name of the {@link Order}
	 * @param type
	 * 		the type of the {@link Order}
	 *
	 * @return the newly created {@link Order}
	 */
	public static Order createOrder(String id, String name, String type) {
		return createOrder(id, name, type, new Date(), State.CREATED);
	}

	/**
	 * Creates an {@link Order} with the given values and adds a {@link ParameterBag} by calling {@link
	 * #createParameterBag(String, String, String)}
	 *
	 * @param id
	 * 		the id of the {@link Order}
	 * @param name
	 * 		the name of the {@link Order}
	 * @param type
	 * 		the type of the {@link Order}
	 * @param date
	 * 		the date of the {@link Order}
	 * @param state
	 * 		the {@link State} of the {@link Order}
	 *
	 * @return the newly created {@link Order}
	 */
	public static Order createOrder(String id, String name, String type, Date date, State state) {

		Order order = new Order(id, name, type, date, state);
		ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		order.addParameterBag(bag);

		order.setPolicyDefs(createPolicyDefs());

		return order;
	}

	/**
	 * Creates a simple {@link PolicyDefs} object with a "ObjectPolicy" with value "java:java.lang.Object"
	 *
	 * @return the created {@link PolicyDefs}
	 */
	public static PolicyDefs createPolicyDefs() {
		PolicyDefs policyDefs = new PolicyDefs();
		PolicyDef policyDef = new JavaPolicyDef("ObjectPolicy", Object.class.getName());
		policyDefs.addOrUpdate(policyDef);
		return policyDefs;
	}

	/**
	 * Creates a list of {@link Order Orders} with the given values and adds a {@link ParameterBag} by calling {@link
	 * #createParameterBag(String, String, String)}
	 *
	 * @param idStart
	 * 		id range start
	 * @param count
	 * 		the number of elements to create
	 * @param idPrefix
	 * 		the prefix to generate IDs for the {@link Order Orders}
	 * @param name
	 * 		the name of the {@link Order}
	 * @param type
	 * 		the type of the {@link Order}
	 *
	 * @return the list of newly created {@link Order Orders}
	 */
	public static List<Order> createOrders(int idStart, int count, String idPrefix, String name, String type) {
		List<Order> orders = new ArrayList<>();
		for (int i = 0; i < count; i++) {
			String id = StringHelper.normalizeLength(String.valueOf((i + idStart)), 8, true, '0');
			orders.add(createOrder(idPrefix + id, name + " " + i, type));
		}
		return orders;
	}

	/**
	 * Creates a list of {@link Activity Activities} with the given values and adds a {@link ParameterBag} by calling
	 * {@link #createParameterBag(String, String, String)}
	 *
	 * @param idStart
	 * 		id range start
	 * @param count
	 * 		the number of elements to create
	 * @param idPrefix
	 * 		the prefix to generate IDs for the {@link Activity Activities}
	 * @param name
	 * 		the name of the {@link Activity}
	 * @param type
	 * 		the type of the {@link Activity}
	 *
	 * @return the list of newly created {@link Activity Activities}
	 */
	public static List<Activity> createActivities(int idStart, int count, String idPrefix, String name, String type,
			TimeOrdering timeOrdering) {
		List<Activity> activities = new ArrayList<>();
		for (int i = 0; i < count; i++) {
			String id = StringHelper.normalizeLength(String.valueOf((i + idStart)), 8, true, '0');
			activities.add(createActivity(idPrefix + id, name + " " + i, type, timeOrdering));
		}
		return activities;
	}

	public static Activity createActivity(String id, String name, String type, TimeOrdering timeOrdering) {

		Activity rootActivity = new Activity(id, name, type, timeOrdering);
		ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		rootActivity.addParameterBag(bag);

		Action action = createAction("action_" + rootActivity.getId(), "Action " + rootActivity.getName(), "Use");
		rootActivity.addElement(action);

		Activity subActivity = new Activity("sub_" + id, "sub_" + name, type, timeOrdering);
		bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		subActivity.addParameterBag(bag);
		rootActivity.addElement(subActivity);

		action = createAction("action_" + id, "Action " + name, "Use");
		subActivity.addElement(action);

		Activity subSubActivity = new Activity("subSub_" + id, "subSub_" + name, type, timeOrdering);
		bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		subSubActivity.addParameterBag(bag);
		subActivity.addElement(subSubActivity);

		action = createAction("action1_" + id, "Action " + name, "Use");
		subSubActivity.addElement(action);

		action = createAction("action2_" + id, "Action " + name, "Use");
		subSubActivity.addElement(action);

		rootActivity.setPolicyDefs(createPolicyDefs());

		return rootActivity;
	}

	public static Action createAction(String id, String name, String type) {
		Action action = new Action(id, name, type);
		action.setResourceId(ACTION_RES_ID);
		action.setResourceType(ACTION_RES_TYPE);
		ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		action.addParameterBag(bag);

		action.addChange(new ValueChange<>(0L, new IntegerValue(0), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(10L, new IntegerValue(10), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(20L, new IntegerValue(20), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(30L, new IntegerValue(30), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(40L, new IntegerValue(20), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(50L, new IntegerValue(10), STATE_INTEGER_ID));
		action.addChange(new ValueChange<>(60L, new IntegerValue(0), STATE_INTEGER_ID));

		return action;
	}

	public static Action createAction(String id, String name, String type, String resourceId, String resourceType) {
		Action action = createAction(id, name, type);
		action.setResourceId(resourceId);
		action.setResourceType(resourceType);
		return action;
	}

	/**
	 * Creates a {@link ParameterBag} with the given values and calls {@link #addAllParameters(ParameterBag)} to add
	 * {@link Parameter}s
	 *
	 * @param id
	 * 		the id of the {@link ParameterBag}
	 * @param name
	 * 		the name of the {@link ParameterBag}
	 * @param type
	 * 		the type of the {@link ParameterBag}
	 *
	 * @return the newly created {@link ParameterBag}
	 */
	public static ParameterBag createParameterBag(String id, String name, String type) {
		ParameterBag bag = new ParameterBag(id, name, type);
		addAllParameters(bag);
		return bag;
	}

	/**
	 * Adds the following {@link Parameter}s to the given {@link ParameterBag}:
	 * <ul>
	 * <li>BooleanParameter - true</li>
	 * <li>FloatParameter - 44.3</li>
	 * <li>IntegerParameter - 77</li>
	 * <li>LongParameter - 4453234566L</li>
	 * <li>StringParameter - "Strolch"</li>
	 * <li>DateParameter - 1354295525628L</li>
	 * <li>StringListParameter - Hello, World</li>
	 * </ul>
	 *
	 * @param bag
	 */
	public static void addAllParameters(ParameterBag bag) {

		BooleanParameter boolParam = new BooleanParameter(PARAM_BOOLEAN_ID, PARAM_BOOLEAN_NAME, true);
		boolParam.setIndex(1);
		bag.addParameter(boolParam);

		FloatParameter floatParam = new FloatParameter(PARAM_FLOAT_ID, PARAM_FLOAT_NAME, 44.3);
		floatParam.setIndex(2);
		bag.addParameter(floatParam);

		IntegerParameter integerParam = new IntegerParameter(PARAM_INTEGER_ID, PARAM_INTEGER_NAME, 77);
		integerParam.setIndex(3);
		bag.addParameter(integerParam);

		LongParameter longParam = new LongParameter(PARAM_LONG_ID, PARAM_LONG_NAME, 4453234566L);
		longParam.setIndex(4);
		bag.addParameter(longParam);

		StringParameter stringParam = new StringParameter(PARAM_STRING_ID, PARAM_STRING_NAME, "Strolch");
		stringParam.setIndex(5);
		bag.addParameter(stringParam);

		DateParameter dateParam = new DateParameter(PARAM_DATE_ID, PARAM_DATE_NAME, new Date(1354295525628L));
		dateParam.setIndex(6);
		bag.addParameter(dateParam);

		ArrayList<String> stringList = new ArrayList<>();
		stringList.add("Hello");
		stringList.add("World");
		StringListParameter stringListP = new StringListParameter(PARAM_LIST_STRING_ID, PARAM_LIST_STRING_NAME,
				stringList);
		stringListP.setIndex(7);
		bag.addParameter(stringListP);

		DurationParameter durationParam = new DurationParameter(PARAM_DURATION_ID, PARAM_DURATION_NAME,
				ISO8601FormatFactory.getInstance().getDurationFormat().parse("P1D"));
		durationParam.setIndex(8);
		bag.addParameter(durationParam);

		ArrayList<Integer> intList = new ArrayList<>();
		intList.add(5);
		intList.add(10);
		intList.add(15);
		IntegerListParameter intListP = new IntegerListParameter(PARAM_LIST_INTEGER_ID, PARAM_LIST_INTEGER_NAME,
				intList);
		intListP.setIndex(9);
		bag.addParameter(intListP);

		ArrayList<Double> floatList = new ArrayList<>();
		floatList.add(6.0);
		floatList.add(11.0);
		floatList.add(16.0);
		FloatListParameter floatListP = new FloatListParameter(PARAM_LIST_FLOAT_ID, PARAM_LIST_FLOAT_NAME, floatList);
		floatListP.setIndex(10);
		bag.addParameter(floatListP);

		ArrayList<Long> longList = new ArrayList<>();
		longList.add(7L);
		longList.add(12L);
		longList.add(17L);
		LongListParameter longListP = new LongListParameter(PARAM_LIST_LONG_ID, PARAM_LIST_LONG_NAME, longList);
		longListP.setIndex(11);
		bag.addParameter(longListP);
	}

	private static String randomValue(Random rand, String[] values) {
		return values[rand.nextInt(values.length)];
	}

	public static Audit randomAudit() {

		Random rand = new Random(234234L);
		String[] usernames = new String[] { "bob", "alice", "jenny" };
		String[] firstnames = new String[] { "Bob", "Alice", "Jenny" };
		String[] lastnames = new String[] { "Richards", "Kennedy", "Davids" };
		String[] types = new String[] { Tags.RESOURCE, Tags.ORDER, Tags.AUDIT };
		String[] subTypes = new String[] { "Ball", "Something", "Foo", "Bar" };
		String[] actions = new String[] { "AddResourceService",
				"UpdateResourceService",
				"RemoveResourceService",
				"AddOrderService",
				"UpdateOrderService",
				"RemoveOrderService" };

		Audit audit = new Audit();
		audit.setId(StringHelper.getUniqueIdLong());
		audit.setUsername(randomValue(rand, usernames));
		audit.setFirstname(randomValue(rand, firstnames));
		audit.setLastname(randomValue(rand, lastnames));
		audit.setDate(new Date(rand.nextInt(5000)));
		audit.setElementType(randomValue(rand, types));
		audit.setElementSubType(randomValue(rand, subTypes));
		audit.setElementAccessed(StringHelper.getUniqueId());
		audit.setNewVersion(new Date(rand.nextInt(5000)));
		audit.setAction(randomValue(rand, actions));
		audit.setAccessType(AccessType.values()[rand.nextInt(AccessType.values().length)]);

		return audit;
	}
}
