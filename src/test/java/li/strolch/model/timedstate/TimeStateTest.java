package li.strolch.model.timedstate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.DoubleValue;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

public class TimeStateTest {

	private ITimedState<DoubleValue> state = new TimedState<DoubleValue>();

	final DoubleValue expectedValue1 = new DoubleValue(Double.valueOf(100D));
	final DoubleValue expectedValue2 = new DoubleValue(Double.valueOf(200D));

	final Long t0 = Long.valueOf(0);
	final Long t10 = Long.valueOf(10);
	final Long t20 = Long.valueOf(20);
	final Long t30 = Long.valueOf(30);
	final Long t100 = Long.valueOf(100);

	@Before
	public void before() {

		final IValueChange<DoubleValue> change1 = new ValueChange<DoubleValue>(this.t10, this.expectedValue1);
		this.state.applyChange(change1);

		final ITimeValue<DoubleValue> stateAt9 = this.state.getStateAt(9L);
		assertNull(stateAt9);

		final ITimeValue<DoubleValue> stateAt11 = this.state.getStateAt(11L);
		assertNotNull(stateAt11);
		assertEquals(true, stateAt11.getValue().matches(this.expectedValue1));

		final IValueChange<DoubleValue> change2 = new ValueChange<DoubleValue>(this.t30, this.expectedValue1);
		this.state.applyChange(change2);

		final ITimeValue<DoubleValue> stateAt31 = this.state.getStateAt(31L);
		assertNotNull(stateAt31);
		assertEquals(true, stateAt31.getValue().matches(this.expectedValue2));
	}

	@Test
	public void testGetNextMatch() {

		ITimeValue<DoubleValue> nextMatch = this.state.getNextMatch(this.t0, this.expectedValue1);
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

		ITimeValue<DoubleValue> previousMatch = this.state.getPreviousMatch(this.t100, this.expectedValue2);
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
