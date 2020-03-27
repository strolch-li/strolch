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
package li.strolch.utils.collections;

import static org.junit.Assert.*;

import java.util.Date;

import li.strolch.utils.dbc.DBC.DbcException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class DateRangeTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void testFrom() {
		Date now = new Date();
		DateRange dateRange = new DateRange();
		dateRange.from(now, true);
		assertEquals(now, dateRange.getFromDate());
		try {
			dateRange.getToDate();
			fail("Should fail, as toDate is not set!");
		} catch (IllegalStateException e) {
			if (!e.getMessage().equals("toDate is unbounded, check with isToBounded()"))
				fail("Should fail with different exception message, as toDate is not set!");
		}
		assertFalse(dateRange.isUnbounded());
		assertFalse(dateRange.isBounded());
	}

	/**
	 * Test method for {@link li.strolch.utils.collections.DateRange#to(java.util.Date, boolean)}.
	 */
	@Test
	public void testTo() {
		Date now = new Date();
		DateRange dateRange = new DateRange();
		dateRange.to(now, true);
		assertEquals(now, dateRange.getToDate());
		try {
			dateRange.getFromDate();
			fail("Should fail, as fromDate is not set!");
		} catch (IllegalStateException e) {
			if (!e.getMessage().equals("fromDate is unbounded, check with isFromBounded()"))
				fail("Should fail with different exception message, as fromDate is not set!");
		}
		assertFalse(dateRange.isUnbounded());
		assertFalse(dateRange.isBounded());
	}

	/**
	 * Test method for {@link li.strolch.utils.collections.DateRange#to(java.util.Date, boolean)}.
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
		this.exception.expect(DbcException.class);
		Date from = new Date(10);
		Date to = new Date(20);
		DateRange dateRange = new DateRange();
		dateRange.from(to, true).to(from, true);
	}

	/**
	 * Test method for {@link li.strolch.utils.collections.DateRange#isDate()}.
	 */
	@Test
	public void testIsDate() {

		Date from = new Date(10);
		Date to = new Date(20);
		DateRange dateRange = new DateRange();
		dateRange.from(from, false).to(to, false);
		assertFalse(dateRange.isDate());

		dateRange = new DateRange();
		dateRange.from(from, false).to(from, false);
		assertTrue(dateRange.isDate());
	}

	/**
	 * Test method for {@link li.strolch.utils.collections.DateRange#contains(java.util.Date)}.
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
