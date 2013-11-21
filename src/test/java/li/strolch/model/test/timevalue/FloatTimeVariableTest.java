package li.strolch.model.test.timevalue;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.SortedSet;

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
		this.timeVariable = new TimeVariable<DoubleValue>();
		for (long i = 0; i < MAX; i += STEP) {
			this.timeVariable.setValueAt(Long.valueOf(i), new DoubleValue(i));
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<DoubleValue> valueAt = this.timeVariable.getValueAt(PICK);
		assertEquals(PICK.doubleValue(), valueAt.getValue().getValue(), 0.0001);
	}

	/**
	 * test, that the future values start with the PICK time and are ascending
	 */
	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<DoubleValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;
		Double expectedValue = PICK.doubleValue();
		for (ITimeValue<DoubleValue> value : futureValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new DoubleValue(expectedValue)));
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
		Collection<ITimeValue<DoubleValue>> pastValues = this.timeVariable.getPastValues(MAX);
		Long expectedTime = 0L;
		Double expectedValue = expectedTime.doubleValue();
		for (ITimeValue<DoubleValue> value : pastValues) {
			assertEquals(expectedTime, value.getTime());
			assertTrue(value.getValue().matches(new DoubleValue(expectedValue)));
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
		this.timeVariable.applyChange(change);

		Collection<ITimeValue<DoubleValue>> futureValues = this.timeVariable.getFutureValues(PICK);
		Long expectedTime = PICK;

		IValue<Double> expectedValue = new DoubleValue(PICK.doubleValue() + change.getValue().getValue());

		for (ITimeValue<DoubleValue> value : futureValues) {
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

		this.timeVariable = new TimeVariable<DoubleValue>();

		DoubleValue doubleValue = new DoubleValue(STEP.doubleValue());

		IValueChange<DoubleValue> change = new ValueChange<DoubleValue>(PICK, doubleValue);
		this.timeVariable.applyChange(change);

		ITimeValue<DoubleValue> actual = this.timeVariable.getValueAt(PICK); 
		assertNotNull(actual); 	
		
		IValue<Double> expectedValue = new DoubleValue(STEP.doubleValue());
		assertEquals(true, actual.getValue().matches(expectedValue));
	}

	/**
	 * test that successors matching the values of their predecessors are
	 * removed
	 */
	@Test
	public void testCompact() {

		this.timeVariable = new TimeVariable<DoubleValue>();
		for (Long i = 0L; i < MAX; i += STEP) {
			this.timeVariable.setValueAt(i, new DoubleValue(STEP.doubleValue()));
		}
		// call
		this.timeVariable.compact();
		// check
		SortedSet<ITimeValue<DoubleValue>> futureValues = this.timeVariable.getFutureValues(0L);
		assertEquals(1, futureValues.size());
	}

}
