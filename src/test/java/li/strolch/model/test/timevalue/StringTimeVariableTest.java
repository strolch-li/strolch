package li.strolch.model.test.timevalue;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.StringSetValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

import org.junit.Before;
import org.junit.Test;

public class StringTimeVariableTest {

	private static final Long MAX = 100L;
	private static final Long STEP = 10L;
	private static final Long PICK = 50L;

	private TimeVariable<IValue<Set<AString>>> timeVariable;

	private Map<Long, StringSetValue> testSets = new HashMap<Long, StringSetValue>();

	@Before
	public void init() {
		this.timeVariable = new TimeVariable<IValue<Set<AString>>>();
		for (Long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<AString>();
			StringSetValue testValue = new StringSetValue(testSet);
			this.testSets.put(i, testValue);
			testSet.add(new AString("string " + i)); //$NON-NLS-1$
			this.timeVariable.setValueAt(i, new StringSetValue(testSet));
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<IValue<Set<AString>>> valueAt = this.timeVariable.getValueAt(PICK);
		assertEquals(true, valueAt.getValue().matches(this.testSets.get(PICK)));
	}

	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<IValue<Set<AString>>>> futureValues = this.timeVariable.getFutureValues(PICK);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : futureValues) {
			Long time = iTimeValue.getTime();
			assertEquals(true, time >= PICK);
			assertNotNull(iTimeValue.getValue());
			assertEquals(true, iTimeValue.getValue().matches(this.testSets.get(time)));
		}
	}

	@Test
	public void testGetPastValues() {
		Collection<ITimeValue<IValue<Set<AString>>>> pastValues = this.timeVariable.getPastValues(PICK);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : pastValues) {
			Long time = iTimeValue.getTime();
			assertEquals(true, time < PICK);
			assertNotNull(iTimeValue.getValue());
			assertEquals(true, iTimeValue.getValue().matches(this.testSets.get(time)));
		}
	}

	@Test
	public void testApplyChange() {

		Set<AString> testSet = new HashSet<AString>();
		testSet.add(new AString("Martin")); //$NON-NLS-1$
		StringSetValue testValue = new StringSetValue(testSet);

		this.timeVariable = new TimeVariable<IValue<Set<AString>>>();
		this.timeVariable.setValueAt(PICK, testValue);

		IValue<Set<AString>> inverseTestValue = testValue.getInverse();
		IValueChange<IValue<Set<AString>>> change = new ValueChange<IValue<Set<AString>>>(PICK, inverseTestValue);
		this.timeVariable.applyChange(change);

		// check the future values
		Collection<ITimeValue<IValue<Set<AString>>>> futureValues = this.timeVariable.getFutureValues(0L);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : futureValues) {
			System.out.println("++ " + iTimeValue); //$NON-NLS-1$
		}

		assertEquals(1, futureValues.size()); // a empty one is left

	}

	@Test
	public void testCompact() {

		this.timeVariable = new TimeVariable<IValue<Set<AString>>>();
		for (Long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<AString>();
			StringSetValue testValue = new StringSetValue(testSet);
			this.testSets.put(i, testValue);
			testSet.add(new AString("same string")); //$NON-NLS-1$
			this.timeVariable.setValueAt(i, new StringSetValue(testSet));
		}

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesInitial = this.timeVariable.getFutureValues(0L);
		assertEquals(true, valuesInitial.size() > 1);

		this.timeVariable.compact();

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesCompacted = this.timeVariable.getFutureValues(0L);
		assertEquals(1, valuesCompacted.size());

	}

}
