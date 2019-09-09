package li.strolch.utils.time;

import static java.time.ZoneId.systemDefault;
import static li.strolch.utils.time.PeriodHelper.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import org.junit.Test;

public class PeriodHelperTest {

	@Test
	public void shouldCalcHalfADay() {
		assertEquals(0.5, daysIn(PeriodDuration.parse("PT12H")), 0.0);
	}

	@Test
	public void shouldCalc1Day1() {
		assertEquals(1.0, daysIn(PeriodDuration.parse("P1D")), 0.0);
	}

	@Test
	public void shouldCalc1Day2() {
		assertEquals(1.0, daysIn(PeriodDuration.parse("PT24H")), 0.0);
	}

	@Test
	public void shouldCalc1AndAHalfDays() {
		assertEquals(1.5, daysIn(PeriodDuration.parse("PT36H")), 0.0);
	}

	@Test
	public void shouldCalc2Days1() {
		assertEquals(2.0, daysIn(PeriodDuration.parse("P2D")), 0.0);
	}

	@Test
	public void shouldCalc2Days2() {
		assertEquals(2.0, daysIn(PeriodDuration.parse("PT48H")), 0.0);
	}

	@Test
	public void shouldCalc2Days3() {
		assertEquals(2.0, daysIn(PeriodDuration.parse("P1DT24H")), 0.0);
	}

	@Test
	public void shouldCalc2AndAHalfDays0() {
		assertEquals(2.5, daysIn(PeriodDuration.parse("P1DT36H")), 0.0);
	}

	@Test
	public void shouldCalc3Days() {
		assertEquals(3.0, daysIn(PeriodDuration.parse("PT71H60M")), 0.0);
	}

	@Test
	public void shouldCalc2Days23H() {
		assertEquals(2.958333333, daysIn(PeriodDuration.parse("PT71H")), 0.00001);
	}

	@Test
	public void shouldCalcDays() {
		assertEquals(30, daysIn(PeriodDuration.parse("P1M")), 0.0);
	}

	@Test
	public void shouldCalcShiftDays1() {
		ZonedDateTime past = ZonedDateTime.now().minusDays(35);
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P1M");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		assertTrue(shiftedDate.isAfter(now.minusDays(10)));
		assertTrue(shiftedDate.isBefore(now));
	}

	@Test
	public void shouldCalcShiftDays2() {
		ZonedDateTime past = ZonedDateTime
				.parse("2007-12-03T10:15:30+01:00", DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(systemDefault()));
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P1M");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		// since P1M ist = 30 days, we have a rather inexact match, but it must certainly be
		// after now() - P1M + 5
		// before now()
		assertTrue(shiftedDate.isAfter(now.minusDays(35)));
		assertTrue(shiftedDate.isBefore(now));
	}

	@Test
	public void shouldCalcShiftDays3() {
		ZonedDateTime past = ZonedDateTime.now().minusDays(20);
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P7D");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		assertTrue(shiftedDate.isAfter(now.minusDays(9)));
		assertTrue(shiftedDate.isBefore(now));
	}

	@Test
	public void shouldCalcShiftDays4() {
		ZonedDateTime past = ZonedDateTime
				.parse("2007-12-03T10:15:30+01:00", DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(systemDefault()));
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P7D");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		// since we are many years and months before now, and a year is 356 days and a month is 28 days, we inexact, but at least we must be a more than P7D before now
		assertTrue(shiftedDate.isBefore(now));
	}
}
