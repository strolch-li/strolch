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

import static org.junit.Assert.*;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.ValueChange;
import org.junit.Before;
import org.junit.Test;

public class TimeStateTest {

	private final ITimedState<FloatValue> state = new TimedState<>();

	final FloatValue expectedValue1 = new FloatValue(100D);
	final FloatValue expectedValue2 = new FloatValue(200D);

	final Long t0 = 0L;
	final Long t10 = 10L;
	final Long t20 = 20L;
	final Long t30 = 30L;
	final Long t100 = 100L;

	@Before
	public void before() {

		final IValueChange<FloatValue> change1 = new ValueChange<>(this.t10, this.expectedValue1);
		this.state.applyChange(change1, true);

		final ITimeValue<FloatValue> stateAt9 = this.state.getStateAt(9L);
		assertNull(stateAt9);

		final ITimeValue<FloatValue> stateAt11 = this.state.getStateAt(11L);
		assertNotNull(stateAt11);
		assertTrue(stateAt11.getValue().matches(this.expectedValue1));

		final IValueChange<FloatValue> change2 = new ValueChange<>(this.t30, this.expectedValue1);
		this.state.applyChange(change2, true);

		final ITimeValue<FloatValue> stateAt31 = this.state.getStateAt(31L);
		assertNotNull(stateAt31);
		assertTrue(stateAt31.getValue().matches(this.expectedValue2));
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
