package li.strolch.utils;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class RoundRobinIntTest {

	@Test
	public void shouldDoRoundRobin0_2() {
		RoundRobinInt rr = new RoundRobinInt(0,2);
		assertEquals(0, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
		assertEquals(0, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
	}
	@Test
	public void shouldDoRoundRobin1_2() {
		RoundRobinInt rr = new RoundRobinInt(1,2);
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
	}
	@Test
	public void shouldDoRoundRobinNeg10_2() {
		RoundRobinInt rr = new RoundRobinInt(-10,2);
		assertEquals(-10, rr.next());
		assertEquals(-9, rr.next());
		assertEquals(-8, rr.next());
		assertEquals(-7, rr.next());
		assertEquals(-6, rr.next());
		assertEquals(-5, rr.next());
		assertEquals(-4, rr.next());
		assertEquals(-3, rr.next());
		assertEquals(-2, rr.next());
		assertEquals(-1, rr.next());
		assertEquals(0, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
		assertEquals(-10, rr.next());
	}
}
