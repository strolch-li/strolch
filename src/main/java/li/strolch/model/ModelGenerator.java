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

import ch.eitchnet.utils.helper.StringHelper;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.model.timedstate.BooleanTimedState;
import li.strolch.model.timedstate.FloatTimedState;
import li.strolch.model.timedstate.IntegerTimedState;
import li.strolch.model.timedstate.StringSetTimedState;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.BooleanValue;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;
import li.strolch.model.timevalue.impl.ValueChange;

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

    public static final String STATE_FLOAT_ID = "@state1";
    public static final String STATE_FLOAT_NAME = "Float State";

    public static final String STATE_INTEGER_ID = "@state2";
    public static final String STATE_INTEGER_NAME = "Float State";

    public static final String STATE_STRING_ID = "@state3";
    public static final String STATE_STRING_NAME = "Float State";

    public static final String STATE_BOOLEAN_ID = "@state4";
    public static final String STATE_BOOLEAN_NAME = "Float State";

    public static final long STATE_TIME_0 = 0L;
    public static final long STATE_TIME_10 = 10L;
    public static final long STATE_TIME_20 = 20L;
    public static final long STATE_TIME_30 = 30L;

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

    /**
     * Creates an {@link Resource} with the given values and adds a {@link ParameterBag} by calling
     * {@link #createParameterBag(String, String, String)}
     *
     * @param id the id of the {@link Resource}
     * @param name the name of the {@link Resource}
     * @param type the type of the {@link Resource}
     *
     * @return the newly created {@link Resource}
     */
    public static Resource createResource(String id, String name, String type) {
        Resource resource = new Resource(id, name, type);
        ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
        resource.addParameterBag(bag);
        addTimedStates(resource);

        return resource;
    }

    /**
     * Creates {@link StrolchTimedState} instances and adds them to the {@link Resource}
     *
     * @param resource the resource to which to addd the newly created {@link StrolchTimedState}
     */
    public static void addTimedStates(Resource resource) {

        // float state
        FloatTimedState floatTimedState = new FloatTimedState(STATE_FLOAT_ID, STATE_FLOAT_NAME);
        floatTimedState.applyChange(new ValueChange<>(STATE_TIME_0, new FloatValue(STATE_FLOAT_TIME_0)));
        FloatValue floatValueChange = new FloatValue(STATE_FLOAT_TIME_10);
        floatTimedState.applyChange(new ValueChange<>(STATE_TIME_10, floatValueChange));
        floatTimedState.applyChange(new ValueChange<>(STATE_TIME_20, floatValueChange));
        floatTimedState.applyChange(new ValueChange<>(STATE_TIME_30, floatValueChange));
        resource.addTimedState(floatTimedState);

        // integer state
        IntegerTimedState integerTimedState = new IntegerTimedState(STATE_INTEGER_ID, STATE_INTEGER_NAME);
        integerTimedState.applyChange(new ValueChange<>(STATE_TIME_0,
                new IntegerValue(STATE_INTEGER_TIME_0)));
        IntegerValue integerValueChange = new IntegerValue(STATE_INTEGER_TIME_10);
        integerTimedState.applyChange(new ValueChange<>(STATE_TIME_10, integerValueChange));
        integerTimedState.applyChange(new ValueChange<>(STATE_TIME_20, integerValueChange));
        integerTimedState.applyChange(new ValueChange<>(STATE_TIME_30, integerValueChange));
        resource.addTimedState(integerTimedState);

        // boolean state
        BooleanTimedState booleanTimedState = new BooleanTimedState(STATE_BOOLEAN_ID, STATE_BOOLEAN_NAME);
        booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_0,
                new BooleanValue(STATE_BOOLEAN_TIME_0)));
        BooleanValue booleanValueChange = new BooleanValue(STATE_BOOLEAN_TIME_10);
        booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_10, booleanValueChange));
        booleanValueChange = booleanValueChange.getInverse();
        booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_20, booleanValueChange));
        booleanValueChange = booleanValueChange.getInverse();
        booleanTimedState.applyChange(new ValueChange<>(STATE_TIME_30, booleanValueChange));
        resource.addTimedState(booleanTimedState);

        // string state
        StringSetTimedState stringTimedState = new StringSetTimedState(STATE_STRING_ID, STATE_STRING_NAME);
        StringSetValue change = new StringSetValue(asSet(STATE_STRING_TIME_0));
        stringTimedState.applyChange(new ValueChange<>(STATE_TIME_0, change));
        change = change.getInverse();
        change.add(asSet(STATE_STRING_TIME_10));
        stringTimedState.applyChange(new ValueChange<>(STATE_TIME_10, change));
        removeInverted(change.getValue());
        change = change.getInverse();
        change.add(asSet(STATE_STRING_TIME_20));
        stringTimedState.applyChange(new ValueChange<>(STATE_TIME_20, change));
        removeInverted(change.getValue());
        change = change.getInverse();
        change.add(asSet(STATE_STRING_TIME_30));
        stringTimedState.applyChange(new ValueChange<>(STATE_TIME_30, change));
        resource.addTimedState(stringTimedState);
    }

    private static Set<AString> asSet(String value) {
        HashSet<AString> hashSet = new HashSet<>();
        hashSet.add(new AString(value));
        return hashSet;
    }

    private static void removeInverted(Set<AString> set) {
        for (Iterator<AString> iter = set.iterator(); iter.hasNext();) {
            AString aString = iter.next();
            if (aString.isInverse()) {
                iter.remove();
            }
        }
    }

    /**
     * Creates a list of {@link Resource Resources} with the given values and adds a {@link ParameterBag} by calling
     * {@link #createParameterBag(String, String, String)}
     *
     * @param idStart id range start
     * @param count the number of elements to create
     * @param idPrefix the prefix to generate IDs for the {@link Resource Resources}
     * @param name the name of the {@link Resource}
     * @param type the type of the {@link Resource}
     *
     * @return the list of newly created {@link Resource Resources}
     */
    public static List<Resource> createResources(int idStart, int count, String idPrefix, String name, String type) {
        List<Resource> resources = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            String id = StringHelper.normalizeLength(String.valueOf((i + idStart)), 8, true, '0');
            resources.add(createResource(idPrefix + "_" + id, name + " " + i, type));
        }
        return resources;
    }

    /**
     * Creates an {@link Order} with the given values and adds a {@link ParameterBag} by calling
     * {@link #createParameterBag(String, String, String)}
     *
     * @param id the id of the {@link Order}
     * @param name the name of the {@link Order}
     * @param type the type of the {@link Order}
     *
     * @return the newly created {@link Order}
     */
    public static Order createOrder(String id, String name, String type) {
        return createOrder(id, name, type, new Date(), State.CREATED);
    }

    /**
     * Creates an {@link Order} with the given values and adds a {@link ParameterBag} by calling
     * {@link #createParameterBag(String, String, String)}
     *
     * @param id the id of the {@link Order}
     * @param name the name of the {@link Order}
     * @param type the type of the {@link Order}
     * @param date the date of the {@link Order}
     * @param state the {@link State} of the {@link Order}
     *
     * @return the newly created {@link Order}
     */
    public static Order createOrder(String id, String name, String type, Date date, State state) {

        Order order = new Order(id, name, type, date, state);
        ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
        order.addParameterBag(bag);

        return order;
    }

    /**
     * Creates a list of {@link Order Orders} with the given values and adds a {@link ParameterBag} by calling
     * {@link #createParameterBag(String, String, String)}
     *
     * @param idStart id range start
     * @param count the number of elements to create
     * @param idPrefix the prefix to generate IDs for the {@link Order Orders}
     * @param name the name of the {@link Order}
     * @param type the type of the {@link Order}
     *
     * @return the list of newly created {@link Order Orders}
     */
    public static List<Order> createOrders(int idStart, int count, String idPrefix, String name, String type) {
        List<Order> orders = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            String id = StringHelper.normalizeLength(String.valueOf((i + idStart)), 8, true, '0');
            orders.add(createOrder(idPrefix + "_" + id, name + " " + i, type));
        }
        return orders;
    }

    /**
     * Creates a {@link ParameterBag} with the given values and calls {@link #addAllParameters(ParameterBag)} to add
     * {@link Parameter}s
     *
     * @param id the id of the {@link ParameterBag}
     * @param name the name of the {@link ParameterBag}
     * @param type the type of the {@link ParameterBag}
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

        ArrayList<String> stringList = new ArrayList<String>();
        stringList.add("Hello");
        stringList.add("World");
        StringListParameter stringListP = new StringListParameter(PARAM_LIST_STRING_ID, PARAM_LIST_STRING_NAME,
                stringList);
        stringListP.setIndex(7);
        bag.addParameter(stringListP);
    }
}
