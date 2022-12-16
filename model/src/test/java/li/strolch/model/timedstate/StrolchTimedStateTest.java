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
package li.strolch.model.timedstate;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.Set;

import li.strolch.model.Resource;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.impl.*;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchTimedStateTest {

	@Test
	public void testFloatState() {

		Resource myRes = createResource("@1", "Test With States", "Stated");

		FloatTimedState floatState = myRes.getTimedState(STATE_FLOAT_ID);
		ITimeValue<FloatValue> valueAt0 = floatState.getTimeEvolution().getValueAt(STATE_TIME_0);
		assertEquals(STATE_FLOAT_TIME_0, valueAt0.getValue().getValue(), 0.0);

		ITimeValue<FloatValue> valueAt10 = floatState.getTimeEvolution().getValueAt(STATE_TIME_10);
		assertEquals(STATE_FLOAT_TIME_10, valueAt10.getValue().getValue(), 0.0);

		ITimeValue<FloatValue> valueAt20 = floatState.getTimeEvolution().getValueAt(STATE_TIME_20);
		assertEquals(STATE_FLOAT_TIME_20, valueAt20.getValue().getValue(), 0.0);

		ITimeValue<FloatValue> valueAt30 = floatState.getTimeEvolution().getValueAt(STATE_TIME_30);
		assertEquals(STATE_FLOAT_TIME_30, valueAt30.getValue().getValue(), 0.0);
	}

	@Test
	public void testFloatListState() {

		Resource myRes = createResource("@1", "Test With States", "Stated");

		FloatListTimedState floatListState = myRes.getTimedState(STATE_FLOAT_LIST_ID);
		ITimeValue<FloatListValue> valueAt0 = floatListState.getTimeEvolution().getValueAt(STATE_TIME_0);
		assertEquals(singletonList(STATE_FLOAT_TIME_0), valueAt0.getValue().getValue());

		ITimeValue<FloatListValue> valueAt10 = floatListState.getTimeEvolution().getValueAt(STATE_TIME_10);
		assertEquals(singletonList(STATE_FLOAT_TIME_0), valueAt10.getValue().getValue());

		ITimeValue<FloatListValue> valueAt20 = floatListState.getTimeEvolution().getValueAt(STATE_TIME_20);
		assertEquals(singletonList(STATE_FLOAT_TIME_0), valueAt20.getValue().getValue());

		ITimeValue<FloatListValue> valueAt30 = floatListState.getTimeEvolution().getValueAt(STATE_TIME_30);
		assertEquals(asList(STATE_FLOAT_TIME_0, STATE_FLOAT_TIME_10, STATE_FLOAT_TIME_20),
				valueAt30.getValue().getValue());
	}

	@Test
	public void testIntegerState() {

		Resource myRes = createResource("@1", "Test With States", "Stated");

		IntegerTimedState integerState = myRes.getTimedState(STATE_INTEGER_ID);
		ITimeValue<IntegerValue> valueAt0 = integerState.getTimeEvolution().getValueAt(STATE_TIME_0);
		assertEquals(STATE_INTEGER_TIME_0, valueAt0.getValue().getValue().intValue());

		ITimeValue<IntegerValue> valueAt10 = integerState.getTimeEvolution().getValueAt(STATE_TIME_10);
		assertEquals(STATE_INTEGER_TIME_10, valueAt10.getValue().getValue().intValue());

		ITimeValue<IntegerValue> valueAt20 = integerState.getTimeEvolution().getValueAt(STATE_TIME_20);
		assertEquals(STATE_INTEGER_TIME_20, valueAt20.getValue().getValue().intValue());

		ITimeValue<IntegerValue> valueAt30 = integerState.getTimeEvolution().getValueAt(STATE_TIME_30);
		assertEquals(STATE_INTEGER_TIME_30, valueAt30.getValue().getValue().intValue());
	}

	@Test
	public void testBooleanState() {

		Resource myRes = createResource("@1", "Test With States", "Stated");

		BooleanTimedState booleanState = myRes.getTimedState(STATE_BOOLEAN_ID);
		ITimeValue<BooleanValue> valueAt0 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_0);
		assertEquals(STATE_BOOLEAN_TIME_0, valueAt0.getValue().getValue());

		ITimeValue<BooleanValue> valueAt10 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_10);
		assertEquals(STATE_BOOLEAN_TIME_10, valueAt10.getValue().getValue());

		ITimeValue<BooleanValue> valueAt20 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_20);
		assertEquals(STATE_BOOLEAN_TIME_20, valueAt20.getValue().getValue());

		ITimeValue<BooleanValue> valueAt30 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_30);
		assertEquals(STATE_BOOLEAN_TIME_30, valueAt30.getValue().getValue());
	}

	@Test
	public void testStringSetState() {

		Resource myRes = createResource("@1", "Test With States", "Stated");

		StringSetTimedState booleanState = myRes.getTimedState(STATE_STRING_ID);
		ITimeValue<StringSetValue> valueAt0 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_0);
		assertEquals(asSet(STATE_STRING_TIME_0), valueAt0.getValue().getValue());

		ITimeValue<StringSetValue> valueAt10 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_10);
		assertEquals(asSet(STATE_STRING_TIME_10), valueAt10.getValue().getValue());

		ITimeValue<StringSetValue> valueAt20 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_20);
		assertEquals(asSet(STATE_STRING_TIME_20), valueAt20.getValue().getValue());

		ITimeValue<StringSetValue> valueAt30 = booleanState.getTimeEvolution().getValueAt(STATE_TIME_30);
		assertEquals(asSet(STATE_STRING_TIME_30), valueAt30.getValue().getValue());
	}

	private static Set<AString> asSet(String value) {
		HashSet<AString> hashSet = new HashSet<>();
		hashSet.add(new AString(value));
		return hashSet;
	}
}
