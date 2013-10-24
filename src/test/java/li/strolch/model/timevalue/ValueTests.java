package li.strolch.model.timevalue;

import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.Set;

import li.strolch.model.timevalue.impl.AString;
import li.strolch.model.timevalue.impl.DoubleValue;
import li.strolch.model.timevalue.impl.IntegerValue;
import li.strolch.model.timevalue.impl.StringSetValue;

import org.junit.Test;

public class ValueTests {

	/**
	 * check, that adding the inverse results in the neutral element (=0)
	 */
	@Test
	public void testDoubleInverse() {
		DoubleValue value = new DoubleValue(10.0d);
		DoubleValue inverse = value.getInverse();
		assertEquals(Double.valueOf(-10.0d), inverse.getValue());
		assertEquals(Double.valueOf(0), value.add(inverse.getValue()).getValue());
	}

	/**
	 * check, that adding the inverse results in the neutral element (=0)
	 */
	@Test
	public void testIntegerInverse() {
		IntegerValue value = new IntegerValue(10);
		IntegerValue inverse = value.getInverse();
		assertEquals(Integer.valueOf(-10), inverse.getValue());
		assertEquals(Integer.valueOf(0), value.add(inverse.getValue()).getValue());
	}

	/**
	 * check, that adding the inverse results in the neutral element (empty Set)
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
