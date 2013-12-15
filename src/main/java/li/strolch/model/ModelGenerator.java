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

import java.util.ArrayList;
import java.util.Date;

import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

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

	public static final String BAG_ID = "@bag01";
	public static final String BAG_NAME = "Test Bag";
	public static final String BAG_TYPE = "TestBag";

	/**
	 * Creates an {@link Resource} with the given values and adds a {@link ParameterBag} by calling
	 * {@link #createParameterBag(String, String, String)}
	 * 
	 * @param id
	 *            the id of the {@link Resource}
	 * @param name
	 *            the name of the {@link Resource}
	 * @param type
	 *            the type of the {@link Resource}
	 * 
	 * @return the newly created {@link Resource}
	 */
	public static Resource createResource(String id, String name, String type) {
		Resource resource = new Resource(id, name, type);
		ParameterBag bag = createParameterBag(BAG_ID, BAG_NAME, BAG_TYPE);
		resource.addParameterBag(bag);

		return resource;
	}

	/**
	 * Creates an {@link Order} with the given values and adds a {@link ParameterBag} by calling
	 * {@link #createParameterBag(String, String, String)}
	 * 
	 * @param id
	 *            the id of the {@link Order}
	 * @param name
	 *            the name of the {@link Order}
	 * @param type
	 *            the type of the {@link Order}
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
	 * @param id
	 *            the id of the {@link Order}
	 * @param name
	 *            the name of the {@link Order}
	 * @param type
	 *            the type of the {@link Order}
	 * @param date
	 *            the date of the {@link Order}
	 * @param state
	 *            the {@link State} of the {@link Order}
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
	 * Creates a {@link ParameterBag} with the given values and calls {@link #addAllParameters(ParameterBag)} to add
	 * {@link Parameter}s
	 * 
	 * @param id
	 *            the id of the {@link ParameterBag}
	 * @param name
	 *            the name of the {@link ParameterBag}
	 * @param type
	 *            the type of the {@link ParameterBag}
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
		bag.addParameter(boolParam);

		FloatParameter floatParam = new FloatParameter(PARAM_FLOAT_ID, PARAM_FLOAT_NAME, 44.3);
		bag.addParameter(floatParam);

		IntegerParameter integerParam = new IntegerParameter(PARAM_INTEGER_ID, PARAM_INTEGER_NAME, 77);
		bag.addParameter(integerParam);

		LongParameter longParam = new LongParameter(PARAM_LONG_ID, PARAM_LONG_NAME, 4453234566L);
		bag.addParameter(longParam);

		StringParameter stringParam = new StringParameter(PARAM_STRING_ID, PARAM_STRING_NAME, "Strolch");
		bag.addParameter(stringParam);

		DateParameter dateParam = new DateParameter(PARAM_DATE_ID, PARAM_DATE_NAME, new Date(1354295525628L));
		bag.addParameter(dateParam);

		ArrayList<String> stringList = new ArrayList<String>();
		stringList.add("Hello");
		stringList.add("World");
		StringListParameter stringListP = new StringListParameter(PARAM_LIST_STRING_ID, PARAM_LIST_STRING_NAME,
				stringList);
		bag.addParameter(stringListP);
	}
}
