/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
import static org.junit.Assert.assertEquals;
import li.strolch.model.parameter.Parameter;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ModelCompareTest {

	@Test
	public void shouldCompareToEqual() {

		ParameterBag bag1 = ModelGenerator.createParameterBag("@1", "@1", "Test");
		ParameterBag bag2 = ModelGenerator.createParameterBag("@1", "@1", "Test");

		Parameter<?> param1;
		Parameter<?> param2;

		param1 = bag1.getParameter(PARAM_BOOLEAN_ID);
		param2 = bag2.getParameter(PARAM_BOOLEAN_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_FLOAT_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_INTEGER_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LONG_ID);
		param2 = bag2.getParameter(PARAM_LONG_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_STRING_ID);
		param2 = bag2.getParameter(PARAM_STRING_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_DATE_ID);
		param2 = bag2.getParameter(PARAM_DATE_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_STRING_ID);
		param2 = bag2.getParameter(PARAM_LIST_STRING_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_LIST_INTEGER_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_LIST_FLOAT_ID);
		assertEquals(0, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_LONG_ID);
		param2 = bag2.getParameter(PARAM_LIST_LONG_ID);
		assertEquals(0, param1.compareTo(param2));
	}

	@Test
	public void shouldCompareToLessThan() {

		ParameterBag bag1 = ModelGenerator.createParameterBag("@1", "@1", "Test");
		ParameterBag bag2 = ModelGenerator.createParameterBag("@1", "@1", "Test");

		Parameter<?> param1;
		Parameter<?> param2;

		param1 = bag1.getParameter(PARAM_BOOLEAN_ID);
		param2 = bag2.getParameter(PARAM_BOOLEAN_ID);
		param1.setValueFromString(Boolean.FALSE.toString());
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_FLOAT_ID);
		param1.setValueFromString("-10.123");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_INTEGER_ID);
		param1.setValueFromString("-10");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LONG_ID);
		param2 = bag2.getParameter(PARAM_LONG_ID);
		param1.setValueFromString("-10876543");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_STRING_ID);
		param2 = bag2.getParameter(PARAM_STRING_ID);
		param1.setValueFromString("a");
		assertEquals(-18, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_DATE_ID);
		param2 = bag2.getParameter(PARAM_DATE_ID);
		param1.setValueFromString("1970-01-01T00:00:00.000Z");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_STRING_ID);
		param2 = bag2.getParameter(PARAM_LIST_STRING_ID);
		param1.setValueFromString("");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_LIST_INTEGER_ID);
		param1.setValueFromString("1");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_LIST_FLOAT_ID);
		param1.setValueFromString("1.0");
		assertEquals(-1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_LONG_ID);
		param2 = bag2.getParameter(PARAM_LIST_LONG_ID);
		param1.setValueFromString("10");
		assertEquals(-1, param1.compareTo(param2));
	}

	@Test
	public void shouldCompareToGreaterThan() {

		ParameterBag bag1 = ModelGenerator.createParameterBag("@1", "@1", "Test");
		ParameterBag bag2 = ModelGenerator.createParameterBag("@1", "@1", "Test");

		Parameter<?> param1;
		Parameter<?> param2;

		param1 = bag1.getParameter(PARAM_BOOLEAN_ID);
		param2 = bag2.getParameter(PARAM_BOOLEAN_ID);
		param2.setValueFromString(Boolean.FALSE.toString());
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_FLOAT_ID);
		param2.setValueFromString("-10.123");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_INTEGER_ID);
		param2.setValueFromString("-10");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LONG_ID);
		param2 = bag2.getParameter(PARAM_LONG_ID);
		param2.setValueFromString("-10876543");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_STRING_ID);
		param2 = bag2.getParameter(PARAM_STRING_ID);
		param2.setValueFromString("a");
		assertEquals(18, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_DATE_ID);
		param2 = bag2.getParameter(PARAM_DATE_ID);
		param2.setValueFromString("1970-01-01T00:00:00.000Z");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_STRING_ID);
		param2 = bag2.getParameter(PARAM_LIST_STRING_ID);
		param2.setValueFromString("");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_INTEGER_ID);
		param2 = bag2.getParameter(PARAM_LIST_INTEGER_ID);
		param2.setValueFromString("1");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_FLOAT_ID);
		param2 = bag2.getParameter(PARAM_LIST_FLOAT_ID);
		param2.setValueFromString("1.0");
		assertEquals(1, param1.compareTo(param2));

		param1 = bag1.getParameter(PARAM_LIST_LONG_ID);
		param2 = bag2.getParameter(PARAM_LIST_LONG_ID);
		param2.setValueFromString("10");
		assertEquals(1, param1.compareTo(param2));
	}
}
