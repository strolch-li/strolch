/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package ch.eitchnet.utils.collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ch.eitchnet.utils.dbc.DBC.DbcException;

public class DateRangeTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void testFrom() {
		Date now = new Date();
		DateRange dateRange = new DateRange();
		dateRange.from(now, true);
		assertEquals(now, dateRange.getFromDate());
		assertNull(dateRange.getToDate());
		assertFalse(dateRange.isUnbounded());
		assertFalse(dateRange.isBounded());
	}

	/**
	 * Test method for {@link ch.eitchnet.utils.collections.DateRange#to(java.util.Date)}.
	 */
	@Test
	public void testTo() {
		Date now = new Date();
		DateRange dateRange = new DateRange();
		dateRange.to(now, true);
		assertEquals(now, dateRange.getToDate());
		assertNull(dateRange.getFromDate());
		assertFalse(dateRange.isUnbounded());
		assertFalse(dateRange.isBounded());
	}

	/**
	 * Test method for {@link ch.eitchnet.utils.collections.DateRange#to(java.util.Date)}.
	 */
	@Test
	public void testFromTo() {
		Date from = new Date();
		Date to = new Date();
		DateRange dateRange = new DateRange();
		dateRange.from(from, true).to(to, true);
		assertEquals(from, dateRange.getFromDate());
		assertEquals(to, dateRange.getToDate());
		assertFalse(dateRange.isUnbounded());
		assertTrue(dateRange.isBounded());
	}

	@Test
	public void shouldNotOverlap() {
		exception.expect(DbcException.class);
		Date from = new Date(10);
		Date to = new Date(20);
		DateRange dateRange = new DateRange();
		dateRange.from(to, true).to(from, true);
	}

	/**
	 * Test method for {@link ch.eitchnet.utils.collections.DateRange#contains(java.util.Date)}.
	 */
	@Test
	public void testContains() {
		Date from = new Date(10);
		Date to = new Date(20);
		DateRange dateRange = new DateRange();
		dateRange.from(from, false).to(to, false);

		Date earlier = new Date(5);
		Date later = new Date(25);
		Date contained = new Date(15);

		assertFalse(dateRange.contains(earlier));
		assertFalse(dateRange.contains(later));
		assertTrue(dateRange.contains(contained));

		assertFalse(dateRange.contains(from));
		assertFalse(dateRange.contains(to));

		dateRange = new DateRange();
		dateRange.from(from, true).to(to, true);
		assertTrue(dateRange.contains(from));
		assertTrue(dateRange.contains(to));

		dateRange = new DateRange();
		dateRange.from(from, false).to(to, true);
		assertFalse(dateRange.contains(from));
		assertTrue(dateRange.contains(to));

		dateRange = new DateRange();
		dateRange.from(from, true).to(to, false);
		assertTrue(dateRange.contains(from));
		assertFalse(dateRange.contains(to));
	}
}
