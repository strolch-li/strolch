package li.strolch.model.timevalue;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;

import junit.framework.Assert;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

/**
 * Basic tests for a {@link TimeVariable} with integer values.
 * 
 * @author martin_smock
 */
public class IntegerTimeVariableTest {

	private static final Long MAX = 100L;
	private static final Integer STEP = 10;
	private static final Long PICK = 50L;

	private TimeVariable<IntegerValue> timeVariable;
	private Map<Long, IntegerValue> expectedValues = new HashMap<Long, IntegerValue>();

	/**
	 * set the values ascending with a difference of STEP
	 */
	@Before
	public void init() {
		timeVariable = new TimeVariable<IntegerValue>();
		for (int i = 0; i < MAX; i += STEP) {
			IntegerValue expectedValue = new IntegerValue(i);
			Long time = Long.valueOf(i);
			expectedValues.put(time, expectedValue);
			timeVariable.setValueAt(time, expectedValue);
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<IntegerValue> valueAt = timeVariable.getValueAt(PICK);
		Assert.assertEquals(expectedValues.get(PICK), valueAt.getValue());
	}

	/**
	 * test, that the future values start with the PICK time and are ascending
	 */
	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<IntegerValue>> futureValues = timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;
		Integer expectedValue = PICK.intValue();
		for (ITimeValue<IntegerValue> value : futureValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(value.getValue().matches(new IntegerValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP.intValue();
		}
	}

	/**
	 * test, that the past values time fields start with 0 and are strictly
	 * smaller than PICK
	 */
	@Test
	public void testGetPastValues() {
		Collection<ITimeValue<IntegerValue>> pastValues = timeVariable.getPastValues(MAX);
		Long expectedTime = 0L;
		Integer expectedValue = expectedTime.intValue();
		for (ITimeValue<IntegerValue> value : pastValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(value.getValue().matches(new IntegerValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP.intValue();
		}
	}

	/**
	 * apply a change and check that the future values are all changed
	 */
	@Test
	public void testApplyChange() {

		IntegerValue integerValue = new IntegerValue(STEP.intValue());

		IValueChange<IntegerValue> change = new ValueChange<IntegerValue>(PICK, integerValue);
		timeVariable.applyChange(change);

		Collection<ITimeValue<IntegerValue>> futureValues = timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;

		IValue<Integer> expectedValue = new IntegerValue(PICK.intValue() + change.getValue().getValue());
		for (ITimeValue<IntegerValue> value : futureValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(expectedValue.matches(value.getValue()));
			expectedTime += STEP;
			expectedValue = expectedValue.add(STEP.intValue());
		}
	}

	/**
	 * test that successors matching the values of their predecessors are
	 * removed
	 */
	@Test
	public void testCompact() {
		timeVariable = new TimeVariable<IntegerValue>();
		for (Long i = 0L; i < MAX; i += STEP) {
			timeVariable.setValueAt(i, new IntegerValue(STEP.intValue()));
		}

		// call
		timeVariable.compact();

		// check
		SortedSet<ITimeValue<IntegerValue>> futureValues = timeVariable.getFutureValues(0L);
		Assert.assertEquals(1, futureValues.size());

		ITimeValue<IntegerValue> next = futureValues.iterator().next();
		Assert.assertEquals(Long.valueOf(0), next.getTime());
	}

}
