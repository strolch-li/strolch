/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the li.strolch.model.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.test.query;

import java.util.ArrayList;
import java.util.Date;

import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.IntegerParameter;
import li.strolch.model.parameter.LongParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class ModelBuilder {

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
