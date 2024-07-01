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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.StringSetValue;
import li.strolch.model.timevalue.impl.TimeVariable;
import li.strolch.model.timevalue.impl.ValueChange;

/**
 * Basic tests for a {@link TimeVariable} with {@link StringValue}.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class StringTimeVariableTest {

	private static final Logger logger = LoggerFactory.getLogger(StringTimeVariableTest.class);

	private static final Long MAX = 100L;
	private static final Long STEP = 10L;
	private static final Long PICK = 50L;

	private TimeVariable<IValue<Set<AString>>> timeVariable;

	private final Map<Long, StringSetValue> testSets = new HashMap<>();

	@Before
	public void init() {
		this.timeVariable = new TimeVariable<>();
		for (long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<>();
			StringSetValue testValue = new StringSetValue(testSet);
			this.testSets.put(i, testValue);
			testSet.add(new AString("string " + i));
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

		Set<AString> testSet = new HashSet<>();
		testSet.add(new AString("Martin"));
		StringSetValue testValue = new StringSetValue(testSet);

		this.timeVariable = new TimeVariable<>();
		this.timeVariable.setValueAt(PICK, testValue);

		IValue<Set<AString>> inverseTestValue = testValue.getInverse();
		IValueChange<IValue<Set<AString>>> change = new ValueChange<>(PICK, inverseTestValue);
		this.timeVariable.applyChange(change, true);

		// check the future values
		Collection<ITimeValue<IValue<Set<AString>>>> futureValues = this.timeVariable.getFutureValues(0L);
		for (ITimeValue<IValue<Set<AString>>> iTimeValue : futureValues) {
			logger.info("++ {}", iTimeValue);
		}

		assertEquals(1, futureValues.size()); // a empty one is left
	}

	@Test
	public void testCompact() {

		this.timeVariable = new TimeVariable<>();
		for (long i = 0L; i < MAX; i += STEP) {
			Set<AString> testSet = new HashSet<>();
			StringSetValue testValue = new StringSetValue(testSet);
			this.testSets.put(i, testValue);
			testSet.add(new AString("same string"));
			this.timeVariable.setValueAt(i, new StringSetValue(testSet));
		}

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesInitial = this.timeVariable.getFutureValues(0L);
		assertEquals(true, valuesInitial.size() > 1);

		this.timeVariable.compact();

		SortedSet<ITimeValue<IValue<Set<AString>>>> valuesCompacted = this.timeVariable.getFutureValues(0L);
		assertEquals(1, valuesCompacted.size());
	}
}
