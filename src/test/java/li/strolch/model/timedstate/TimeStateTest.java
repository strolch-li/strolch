package li.strolch.model.timedstate;

import junit.framework.Assert;
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

		final IValueChange<DoubleValue> change1 = new ValueChange<DoubleValue>(t10, expectedValue1);
		state.applyChange(change1);

		final ITimeValue<DoubleValue> stateAt9 = state.getStateAt(9L);
		Assert.assertNull(stateAt9);

		final ITimeValue<DoubleValue> stateAt11 = state.getStateAt(11L);
		Assert.assertNotNull(stateAt11);
		Assert.assertEquals(true, stateAt11.getValue().matches(expectedValue1));

		final IValueChange<DoubleValue> change2 = new ValueChange<DoubleValue>(t30, expectedValue1);
		state.applyChange(change2);

		final ITimeValue<DoubleValue> stateAt31 = state.getStateAt(31L);
		Assert.assertNotNull(stateAt31);
		Assert.assertEquals(true, stateAt31.getValue().matches(expectedValue2));
	}

	@Test
	public void testGetNextMatch() {

		ITimeValue<DoubleValue> nextMatch = state.getNextMatch(t0, expectedValue1);
		Assert.assertNotNull(nextMatch);
		Assert.assertEquals(t10, nextMatch.getTime());

		nextMatch = state.getNextMatch(t20, expectedValue1);
		Assert.assertNull(nextMatch);

		nextMatch = state.getNextMatch(t20, expectedValue2);
		Assert.assertNotNull(nextMatch);
		Assert.assertEquals(t30, nextMatch.getTime());

		nextMatch = state.getNextMatch(t30, expectedValue2);
		Assert.assertNotNull(nextMatch);
		Assert.assertEquals(t30, nextMatch.getTime());

		nextMatch = state.getNextMatch(t100, expectedValue1);
		Assert.assertNull(nextMatch);

		nextMatch = state.getNextMatch(t100, expectedValue2);
		Assert.assertNull(nextMatch);

	}

	@Test
	public void testGetPreviousMatch() {

		ITimeValue<DoubleValue> previousMatch = state.getPreviousMatch(t100, expectedValue2);
		Assert.assertNotNull(previousMatch);
		Assert.assertEquals(t30, previousMatch.getTime());

		previousMatch = state.getPreviousMatch(t30, expectedValue2);
		Assert.assertNull(previousMatch);
		
		previousMatch = state.getPreviousMatch(t20, expectedValue2);
		Assert.assertNull(previousMatch);
		
		previousMatch = state.getPreviousMatch(t20, expectedValue1);
		Assert.assertNotNull(previousMatch);
		Assert.assertEquals(t10, previousMatch.getTime());
		
		previousMatch = state.getPreviousMatch(t10, expectedValue1);
		Assert.assertNull(previousMatch);

	}

}
