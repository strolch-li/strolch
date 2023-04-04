/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.model.timevalue;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;

import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

/**
 * Basic tests for a {@link TimeVariable} with integer values.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class IntegerTimeVariableTest {

	private static final Long MAX = 100L;
	private static final Integer STEP = 10;
	private static final Long PICK = 50L;

	private TimeVariable<IntegerValue> timeVariable;
	private final Map<Long, IntegerValue> expectedValues = new HashMap<>();

	/**
	 * set the values ascending with a difference of STEP
	 */
	@Before
	public void init() {
		this.timeVariable = new TimeVariable<>();
		for (int i = 0; i < MAX; i += STEP) {
			IntegerValue expectedValue = new IntegerValue(i);
			long time = (long) i;
			this.expectedValues.put(time, expectedValue);
			this.timeVariable.setValueAt(time, expectedValue);
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<IntegerValue> valueAt = this.timeVariable.getValueAt(PICK);
		assertEquals(this.expectedValues.get(PICK), valueAt.getValue());
	}

	/**
	 * test, that the future values start with the PICK time and are ascending
	 */
	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<IntegerValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;
		int expectedValue = PICK.intValue();
		for (ITimeValue<IntegerValue> value : futureValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new IntegerValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP;
		}
	}

	/**
	 * test, that the past values time fields start with 0 and are strictly smaller than PICK
	 */
	@Test
	public void testGetPastValues() {
		Collection<ITimeValue<IntegerValue>> pastValues = this.timeVariable.getPastValues(MAX);
		Long expectedTime = 0L;
		int expectedValue = expectedTime.intValue();
		for (ITimeValue<IntegerValue> value : pastValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new IntegerValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP;
		}
	}

	/**
	 * apply a change and check that the future values are all changed
	 */
	@Test
	public void testApplyChange() {

		IntegerValue integerValue = new IntegerValue(STEP);

		IValueChange<IntegerValue> change = new ValueChange<>(PICK, integerValue);
		this.timeVariable.applyChange(change, false);

		Collection<ITimeValue<IntegerValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;

		IValue<Integer> expectedValue = new IntegerValue(PICK.intValue() + change.getValue().getValue());
		for (ITimeValue<IntegerValue> value : futureValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(expectedValue.matches(value.getValue()));
			expectedTime += STEP;
			expectedValue = expectedValue.add(STEP);
		}
	}

	/**
	 * test that successors matching the values of their predecessors are removed
	 */
	@Test
	public void testCompact() {
		this.timeVariable = new TimeVariable<>();
		for (long i = 0L; i < MAX; i += STEP) {
			this.timeVariable.setValueAt(i, new IntegerValue(STEP));
		}

		// call
		this.timeVariable.compact();

		// check
		SortedSet<ITimeValue<IntegerValue>> futureValues = this.timeVariable.getFutureValues(0L);
		assertEquals(1, futureValues.size());

		ITimeValue<IntegerValue> next = futureValues.iterator().next();
		assertEquals(Long.valueOf(0), next.getTime());
	}

}
