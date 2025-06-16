/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RoundRobinIntTest {

	@Test
	public void shouldDoRoundRobin0_2() {
		RoundRobinInt rr = new RoundRobinInt(0, 2);
		assertEquals(0, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
		assertEquals(0, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
	}

	@Test
	public void shouldDoRoundRobin1_2() {
		RoundRobinInt rr = new RoundRobinInt(1, 2);
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
		assertEquals(1, rr.next());
		assertEquals(2, rr.next());
	}

	@Test
	public void shouldDoRoundRobinNeg10_2() {
		RoundRobinInt rr = new RoundRobinInt(-10, 2);
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
