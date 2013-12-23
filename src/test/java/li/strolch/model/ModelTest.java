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
import li.strolch.model.visitor.OrderDeepEqualsVisitor;
import li.strolch.model.visitor.ResourceDeepEqualsVisitor;

import org.junit.Test;

@SuppressWarnings("nls")
public class ModelTest {

	@Test
	public void shouldCreateResource() {

		Resource resource = ModelGenerator.createResource("@res01", "Test resource", "MyType");
		ParameterBag bag = resource.getParameterBag(ModelGenerator.BAG_ID);
		validateBag(bag);
	}

	@Test
	public void shouldCreateOrder() {

		Order order = ModelGenerator.createOrder("@ord01", "Test Order", "MyType", new Date(), State.OPEN);
		ParameterBag bag = order.getParameterBag(ModelGenerator.BAG_ID);
		validateBag(bag);
	}

	@Test
	public void shouldPerformDeepResourceEquals() {
		Resource srcRes = ModelGenerator.createResource("@res01", "Test resource", "MyType");
		Resource dstRes = ModelGenerator.createResource("@res01", "Test resource", "MyType");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertTrue("Same Resource should be deep equal!", visitor.isEqual());
	}

	@Test
	public void shouldFailDeepResourceEquals1() {
		Resource srcRes = ModelGenerator.createResource("@res01", "Test resource", "MyType");
		Resource dstRes = ModelGenerator.createResource("@res01", "Test resource", "MyType");
		ParameterBag bag = dstRes.getParameterBag(ModelGenerator.BAG_ID);
		bag.setName("Bla bla");
		FloatParameter fParam = bag.getParameter(ModelGenerator.PARAM_FLOAT_ID);
		fParam.setValue(23434234.234);
		fParam.setName("Ohla");
		ResourceDeepEqualsVisitor visitor = new ResourceDeepEqualsVisitor(srcRes);
		visitor.visit(dstRes);
		assertFalse("Resource should not be same if param is changed!", visitor.isEqual());
		assertEquals("Three changes should be registered", 3, visitor.getMismatchedLocators().size());
	}

	@Test
	public void shouldPerformDeepOrderEquals() {
		Order srcOrder = ModelGenerator.createOrder("@ord01", "Test Order", "MyType", new Date(), State.OPEN);
		Order dstOrder = ModelGenerator.createOrder("@ord01", "Test Order", "MyType", new Date(), State.OPEN);
		OrderDeepEqualsVisitor visitor = new OrderDeepEqualsVisitor(srcOrder);
		visitor.visit(dstOrder);
		assertTrue("Same Order should be deep equal!", visitor.isEqual());
	}

	public static void validateBag(ParameterBag bag) {

		assertNotNull(bag);

		assertEquals(ModelGenerator.BAG_ID, bag.getId());
		assertEquals(ModelGenerator.BAG_NAME, bag.getName());
		assertEquals(ModelGenerator.BAG_TYPE, bag.getType());

		validateParams(bag);
	}

	public static void validateParams(ParameterBag bag) {

		BooleanParameter boolParam = bag.getParameter(ModelGenerator.PARAM_BOOLEAN_ID);
		assertNotNull("Boolean Param missing with id " + ModelGenerator.PARAM_BOOLEAN_ID, boolParam);
		assertEquals(true, boolParam.getValue().booleanValue());

		FloatParameter floatParam = bag.getParameter(ModelGenerator.PARAM_FLOAT_ID);
		assertNotNull("Float Param missing with id " + ModelGenerator.PARAM_FLOAT_ID, floatParam);
		assertEquals(44.3, floatParam.getValue().doubleValue(), 0.0001);

		IntegerParameter integerParam = bag.getParameter(ModelGenerator.PARAM_INTEGER_ID);
		assertNotNull("Integer Param missing with id " + ModelGenerator.PARAM_INTEGER_ID, integerParam);
		assertEquals(77, integerParam.getValue().intValue());

		LongParameter longParam = bag.getParameter(ModelGenerator.PARAM_LONG_ID);
		assertNotNull("Long Param missing with id " + ModelGenerator.PARAM_LONG_ID, longParam);
		assertEquals(4453234566L, longParam.getValue().longValue());

		StringParameter stringParam = bag.getParameter(ModelGenerator.PARAM_STRING_ID);
		assertNotNull("String Param missing with id " + ModelGenerator.PARAM_STRING_ID, stringParam);
		assertEquals("Strolch", stringParam.getValue());

		DateParameter dateParam = bag.getParameter(ModelGenerator.PARAM_DATE_ID);
		assertNotNull("Date Param missing with id " + ModelGenerator.PARAM_DATE_ID, dateParam);
		assertEquals(1354295525628L, dateParam.getValue().getTime());

		StringListParameter stringListP = bag.getParameter(ModelGenerator.PARAM_LIST_STRING_ID);
		assertNotNull("StringList Param missing with id " + ModelGenerator.PARAM_LIST_STRING_ID, stringListP);

		ArrayList<String> stringList = new ArrayList<String>();
		stringList.add("Hello");
		stringList.add("World");
		assertEquals(stringList, stringListP.getValue());
	}
}
