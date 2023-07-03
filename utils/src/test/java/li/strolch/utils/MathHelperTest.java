package li.strolch.utils;

import org.junit.Test;

import static li.strolch.utils.helper.MathHelper.*;
import static org.junit.Assert.*;

public class MathHelperTest {
	@Test
	public void shouldRound1() {
		assertEquals(1.0, toPrecision(1.00001), 0.0);
	}

	@Test
	public void shouldRound2() {
		assertEquals(1.001, toPrecision(1.001), 0.0);
	}

	@Test
	public void shouldRound3() {
		assertEquals(1.333, toPrecision(1.3333333), 0.0);
	}

	@Test
	public void shouldTestToPrecision1() {
		assertEquals(1.0, toPrecision(1.00001, 3), 0.0);
	}

	@Test
	public void shouldTestToPrecision2() {
		assertEquals(1.001, toPrecision(1.001, 3), 0.0);
	}

	@Test
	public void shouldTestToPrecision3() {
		assertEquals(1.333, toPrecision(1.3333333, 3), 0.0);
	}

	@Test
	public void shouldTestEqualsPrecision1() {
		assertTrue(isEqualPrecision(1.333333333, 1.33333333));
	}

	@Test
	public void shouldTestEqualsPrecision2() {
		assertTrue(isEqualPrecision(1.3333333, 1.333, 3));
	}

	@Test
	public void shouldTestEqualsPrecision3() {
		assertTrue(isEqualPrecision(1.033, 1.033435345345, 3));
	}

	@Test
	public void shouldTestSmallerEqualPrecision1() {
		assertTrue(isSmallerEqualPrecision(1.03333, 1.033435345345));
	}

	@Test
	public void shouldTestSmallerEqualPrecision2() {
		assertTrue(isSmallerEqualPrecision(1.033333338, 1.033333335));
	}

	@Test
	public void shouldTestGreaterEqualPrecision1() {
		assertTrue(isGreaterPrecision(1.03343537, 1.03343535));
	}

	@Test
	public void shouldTestGreaterEqualPrecision2() {
		assertTrue(isGreaterPrecision(1.03333335, 1.03333333));
	}

	@Test
	public void shouldTestToPrecisionString1() {
		assertEquals("1.033", toPrecisionString(1.03333335));
	}

	@Test
	public void shouldTestToPrecisionString2() {
		assertEquals("1.034", toPrecisionString(1.0344444));
	}
	@Test
	public void shouldTestGetNumberOfDecimalPlaces1() {
		assertEquals(7, getNumberOfDecimalPlaces(1.0344444));
	}
	@Test
	public void shouldTestGetNumberOfDecimalPlaces2() {
		assertEquals(3, getNumberOfDecimalPlaces(1.034));
	}
	@Test
	public void shouldTestDoubleToString() {
		assertEquals("0.0000000453453335", doubleToString(0.0000000453453335));
	}
}
