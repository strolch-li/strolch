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

import java.util.HashSet;
import java.util.Set;

import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.FloatValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;
import li.strolch.model.timevalue.impl.TimeVariable;

import org.junit.Test;

/**
 * Basic tests for a {@link TimeVariable} with {@link StringValue}.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public class ValueTests {

	/**
	 * check, that adding the inverse returns the neutral element (=0)
	 */
	@Test
	public void testDoubleInverse() {
		FloatValue value = new FloatValue(10.0d);
		FloatValue inverse = value.getInverse();
		assertEquals(Double.valueOf(-10.0d), inverse.getValue());
		assertEquals(Double.valueOf(0), value.add(inverse.getValue()).getValue());
	}

	/**
	 * check, that adding the inverse returns the neutral element (=0)
	 */
	@Test
	public void testIntegerInverse() {
		IntegerValue value = new IntegerValue(10);
		IntegerValue inverse = value.getInverse();
		assertEquals(Integer.valueOf(-10), inverse.getValue());
		assertEquals(Integer.valueOf(0), value.add(inverse.getValue()).getValue());
	}

	/**
	 * check, that adding the inverse returns the neutral element (empty Set)
	 */
	@Test
	public void testStringSetInverse() {
		Set<AString> aStrings = new HashSet<AString>();
		for (int i = 0; i < 10; i++) {
			aStrings.add(new AString("string " + i)); //$NON-NLS-1$
		}
		IValue<Set<AString>> value = new StringSetValue(aStrings);
		IValue<Set<AString>> inverse = value.getInverse();
		assertEquals(true, value.matches(inverse.getInverse()));
		assertEquals(0, value.add(inverse.getValue()).getValue().size());
	}

	/**
	 * check, that the difference left is as expected
	 */
	@Test
	public void testStringSetNearInverse() {

		Set<AString> aStrings1 = new HashSet<AString>();
		for (int i = 0; i < 10; i++) {
			aStrings1.add(new AString("string " + i)); //$NON-NLS-1$
		}
		IValue<Set<AString>> value1 = new StringSetValue(aStrings1);

		Set<AString> aStrings2 = new HashSet<AString>();
		for (int i = 0; i < 9; i++) {
			aStrings2.add(new AString("string " + i, true)); //$NON-NLS-1$
		}
		IValue<Set<AString>> value2 = new StringSetValue(aStrings2);

		assertEquals(false, value1.matches(value2));
		assertEquals(1, value1.add(value2.getValue()).getValue().size());
		assertEquals(10, value1.add(value2.getInverse().getValue()).getValue().size());
	}

}
