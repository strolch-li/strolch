package li.strolch.model.timevalue;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import junit.framework.Assert;
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
		timeVariable = new TimeVariable<IValue<Set<AString>>>();
		for (Long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<AString>();
			StringSetValue testValue = new StringSetValue(testSet);
			testSets.put(i, testValue);
			testSet.add(new AString("string " + i));
			timeVariable.setValueAt(i, new StringSetValue(testSet));
		}
	}

	@Test
	public void testGetValueAt() {
		ITimeValue<IValue<Set<AString>>> valueAt = timeVariable.getValueAt(PICK);
		Assert.assertEquals(true, valueAt.getValue().matches(testSets.get(PICK)));
	}

	@Test
	public void testGetFutureValues() {
		Collection<ITimeValue<IValue<Set<AString>>>> futureValues = timeVariable.getFutureValues(PICK);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : futureValues) {
			Long time = iTimeValue.getTime();
			Assert.assertEquals(true, time >= PICK);
			Assert.assertNotNull(iTimeValue.getValue());
			Assert.assertEquals(true, iTimeValue.getValue().matches(testSets.get(time)));
		}
	}

	@Test
	public void testGetPastValues() {
		Collection<ITimeValue<IValue<Set<AString>>>> pastValues = timeVariable.getPastValues(PICK);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : pastValues) {
			Long time = iTimeValue.getTime();
			Assert.assertEquals(true, time < PICK);
			Assert.assertNotNull(iTimeValue.getValue());
			Assert.assertEquals(true, iTimeValue.getValue().matches(testSets.get(time)));
		}
	}

	@Test
	public void testApplyChange() {

		Set<AString> testSet = new HashSet<AString>();
		testSet.add(new AString("Martin"));
		StringSetValue testValue = new StringSetValue(testSet);

		timeVariable = new TimeVariable<IValue<Set<AString>>>();
		timeVariable.setValueAt(PICK, testValue);

		IValue<Set<AString>> inverseTestValue = testValue.getInverse();
		IValueChange<IValue<Set<AString>>> change = new ValueChange<IValue<Set<AString>>>(PICK, inverseTestValue);
		timeVariable.applyChange(change);

		// check the future values
		Collection<ITimeValue<IValue<Set<AString>>>> futureValues = timeVariable.getFutureValues(0L);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : futureValues) {
			System.out.println("++ " + iTimeValue);
		}
		
		Assert.assertEquals(1, futureValues.size()); // a empty one is left

	}

	@Test
	public void testCompact() {

		timeVariable = new TimeVariable<IValue<Set<AString>>>();
		for (Long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<AString>();
			StringSetValue testValue = new StringSetValue(testSet);
			testSets.put(i, testValue);
			testSet.add(new AString("same string"));
			timeVariable.setValueAt(i, new StringSetValue(testSet));
		}

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesInitial = timeVariable.getFutureValues(0L);
		Assert.assertEquals(true, valuesInitial.size() > 1);

		timeVariable.compact();

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesCompacted = timeVariable.getFutureValues(0L);
		Assert.assertEquals(1, valuesCompacted.size());

	}

}
