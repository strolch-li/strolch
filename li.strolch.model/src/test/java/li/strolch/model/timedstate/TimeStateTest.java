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
package li.strolch.model.timedstate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

public class TimeStateTest {

	private ITimedState<FloatValue> state = new TimedState<FloatValue>();

	final FloatValue expectedValue1 = new FloatValue(Double.valueOf(100D));
	final FloatValue expectedValue2 = new FloatValue(Double.valueOf(200D));

	final Long t0 = Long.valueOf(0);
	final Long t10 = Long.valueOf(10);
	final Long t20 = Long.valueOf(20);
	final Long t30 = Long.valueOf(30);
	final Long t100 = Long.valueOf(100);

	@Before
	public void before() {

		final IValueChange<FloatValue> change1 = new ValueChange<FloatValue>(this.t10, this.expectedValue1);
		this.state.applyChange(change1);

		final ITimeValue<FloatValue> stateAt9 = this.state.getStateAt(9L);
		assertNull(stateAt9);

		final ITimeValue<FloatValue> stateAt11 = this.state.getStateAt(11L);
		assertNotNull(stateAt11);
		assertEquals(true, stateAt11.getValue().matches(this.expectedValue1));

		final IValueChange<FloatValue> change2 = new ValueChange<FloatValue>(this.t30, this.expectedValue1);
		this.state.applyChange(change2);

		final ITimeValue<FloatValue> stateAt31 = this.state.getStateAt(31L);
		assertNotNull(stateAt31);
		assertEquals(true, stateAt31.getValue().matches(this.expectedValue2));
	}

	@Test
	public void testGetNextMatch() {

		ITimeValue<FloatValue> nextMatch = this.state.getNextMatch(this.t0, this.expectedValue1);
		assertNotNull(nextMatch);
		assertEquals(this.t10, nextMatch.getTime());

		nextMatch = this.state.getNextMatch(this.t20, this.expectedValue1);
		assertNull(nextMatch);

		nextMatch = this.state.getNextMatch(this.t20, this.expectedValue2);
		assertNotNull(nextMatch);
		assertEquals(this.t30, nextMatch.getTime());

		nextMatch = this.state.getNextMatch(this.t30, this.expectedValue2);
		assertNotNull(nextMatch);
		assertEquals(this.t30, nextMatch.getTime());

		nextMatch = this.state.getNextMatch(this.t100, this.expectedValue1);
		assertNull(nextMatch);

		nextMatch = this.state.getNextMatch(this.t100, this.expectedValue2);
		assertNull(nextMatch);

	}

	@Test
	public void testGetPreviousMatch() {

		ITimeValue<FloatValue> previousMatch = this.state.getPreviousMatch(this.t100, this.expectedValue2);
		assertNotNull(previousMatch);
		assertEquals(this.t30, previousMatch.getTime());

		previousMatch = this.state.getPreviousMatch(this.t30, this.expectedValue2);
		assertNull(previousMatch);

		previousMatch = this.state.getPreviousMatch(this.t20, this.expectedValue2);
		assertNull(previousMatch);

		previousMatch = this.state.getPreviousMatch(this.t20, this.expectedValue1);
		assertNotNull(previousMatch);
		assertEquals(this.t10, previousMatch.getTime());

		previousMatch = this.state.getPreviousMatch(this.t10, this.expectedValue1);
		assertNull(previousMatch);

	}

}
