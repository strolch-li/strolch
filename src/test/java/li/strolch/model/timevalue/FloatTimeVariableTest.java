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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.SortedSet;

import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

public class FloatTimeVariableTest {

	private static final Long MAX = 100L;
	private static final Long STEP = 10L;
	private static final Long PICK = 50L;

	private TimeVariable<FloatValue> timeVariable;

	/**
	 * set the values ascending with a difference of STEP
	 */
	@Before
	public void init() {
		this.timeVariable = new TimeVariable<FloatValue>();
		for (long i = 0; i < MAX; i += STEP) {
			this.timeVariable.setValueAt(Long.valueOf(i), new FloatValue(i));
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<FloatValue> valueAt = this.timeVariable.getValueAt(PICK);
		assertEquals(PICK.doubleValue(), valueAt.getValue().getValue(), 0.0001);
	}

	/**
	 * test, that the future values start with the PICK time and are ascending
	 */
	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<FloatValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;
		Double expectedValue = PICK.doubleValue();
		for (ITimeValue<FloatValue> value : futureValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new FloatValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP.doubleValue();
		}
	}

	/**
	 * test, that the past values time fields start with 0 and are strictly
	 * smaller than PICK
	 */
	@Test
	public void testGetPastValues() {
		Collection<ITimeValue<FloatValue>> pastValues = this.timeVariable.getPastValues(MAX);
		Long expectedTime = 0L;
		Double expectedValue = expectedTime.doubleValue();
		for (ITimeValue<FloatValue> value : pastValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new FloatValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP.doubleValue();
		}
	}

	/**
	 * apply a change and check that the future values are all changed
	 */
	@Test
	public void testApplyChange() {

		FloatValue doubleValue = new FloatValue(STEP.doubleValue());

		IValueChange<FloatValue> change = new ValueChange<FloatValue>(PICK, doubleValue);
		this.timeVariable.applyChange(change);

		Collection<ITimeValue<FloatValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;

		IValue<Double> expectedValue = new FloatValue(PICK.doubleValue() + change.getValue().getValue());

		for (ITimeValue<FloatValue> value : futureValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(expectedValue.matches(value.getValue()));
			expectedTime += STEP;
			expectedValue = expectedValue.add(STEP.doubleValue());
		}
	}

	/**
	 * apply a change to an empty time variable
	 */
	@Test
	public void testApply2Change() {

		this.timeVariable = new TimeVariable<FloatValue>();

		FloatValue doubleValue = new FloatValue(STEP.doubleValue());

		IValueChange<FloatValue> change = new ValueChange<FloatValue>(PICK, doubleValue);
		this.timeVariable.applyChange(change);

		ITimeValue<FloatValue> actual = this.timeVariable.getValueAt(PICK); 
		assertNotNull(actual); 	
		
		IValue<Double> expectedValue = new FloatValue(STEP.doubleValue());
		assertEquals(true, actual.getValue().matches(expectedValue));
	}

	/**
	 * test that successors matching the values of their predecessors are
	 * removed
	 */
	@Test
	public void testCompact() {

		this.timeVariable = new TimeVariable<FloatValue>();
		for (Long i = 0L; i < MAX; i += STEP) {
			this.timeVariable.setValueAt(i, new FloatValue(STEP.doubleValue()));
		}
		// call
		this.timeVariable.compact();
		// check
		SortedSet<ITimeValue<FloatValue>> futureValues = this.timeVariable.getFutureValues(0L);
		assertEquals(1, futureValues.size());
	}

}
