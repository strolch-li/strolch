package li.strolch.model.timevalue;

import java.util.Collection;
import java.util.SortedSet;

import junit.framework.Assert;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.DoubleValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

public class FloatTimeVariableTest {

	private static final Long MAX = 100L;
	private static final Long STEP = 10L;
	private static final Long PICK = 50L;

	private TimeVariable<DoubleValue> timeVariable;

	/**
	 * set the values ascending with a difference of STEP
	 */
	@Before
	public void init() {
		timeVariable = new TimeVariable<DoubleValue>();
		for (long i = 0; i < MAX; i += STEP) {
			timeVariable.setValueAt(Long.valueOf(i), new DoubleValue(i));
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<DoubleValue> valueAt = timeVariable.getValueAt(PICK);
		Assert.assertEquals(PICK.doubleValue(), valueAt.getValue().getValue());
	}

	/**
	 * test, that the future values start with the PICK time and are ascending
	 */
	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<DoubleValue>> futureValues = timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;
		Double expectedValue = PICK.doubleValue();
		for (ITimeValue<DoubleValue> value : futureValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(value.getValue().matches(new DoubleValue(expectedValue)));
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
		Collection<ITimeValue<DoubleValue>> pastValues = timeVariable.getPastValues(MAX);
		Long expectedTime = 0L;
		Double expectedValue = expectedTime.doubleValue();
		for (ITimeValue<DoubleValue> value : pastValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(value.getValue().matches(new DoubleValue(expectedValue)));
			expectedTime += STEP;
			expectedValue += STEP.doubleValue();
		}
	}

	/**
	 * apply a change and check that the future values are all changed
	 */
	@Test
	public void testApplyChange() {

		DoubleValue doubleValue = new DoubleValue(STEP.doubleValue());

		IValueChange<DoubleValue> change = new ValueChange<DoubleValue>(PICK, doubleValue);
		timeVariable.applyChange(change);

		Collection<ITimeValue<DoubleValue>> futureValues = timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;

		IValue<Double> expectedValue = new DoubleValue(PICK.doubleValue() + change.getValue().getValue());

		for (ITimeValue<DoubleValue> value : futureValues) {
			Assert.assertEquals(expectedTime, value.getTime());
			Assert.assertTrue(expectedValue.matches(value.getValue()));
			expectedTime += STEP;
			expectedValue = expectedValue.add(STEP.doubleValue());
		}
	}

	/**
	 * apply a change to an empty time variable
	 */
	@Test
	public void testApply2Change() {

		timeVariable = new TimeVariable<DoubleValue>();

		DoubleValue doubleValue = new DoubleValue(STEP.doubleValue());

		IValueChange<DoubleValue> change = new ValueChange<DoubleValue>(PICK, doubleValue);
		timeVariable.applyChange(change);

		ITimeValue<DoubleValue> actual = timeVariable.getValueAt(PICK); 
		Assert.assertNotNull(actual); 	
		
		IValue<Double> expectedValue = new DoubleValue(STEP.doubleValue());
		Assert.assertEquals(true, actual.getValue().matches(expectedValue));
	}

	/**
	 * test that successors matching the values of their predecessors are
	 * removed
	 */
	@Test
	public void testCompact() {

		timeVariable = new TimeVariable<DoubleValue>();
		for (Long i = 0L; i < MAX; i += STEP) {
			timeVariable.setValueAt(i, new DoubleValue(STEP.doubleValue()));
		}
		// call
		timeVariable.compact();
		// check
		SortedSet<ITimeValue<DoubleValue>> futureValues = timeVariable.getFutureValues(0L);
		Assert.assertEquals(1, futureValues.size());
	}

}
