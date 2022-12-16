package li.strolch.utils.time;

import static java.time.ZoneId.systemDefault;
import static java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME;
import static li.strolch.utils.time.PeriodHelper.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.time.*;

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
	public void shouldShiftDateYears1() {
		ZonedDateTime date = LocalDate.of(2020, 1, 31).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2021, 1, 31).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P1Y"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateYears2() {
		ZonedDateTime date = LocalDate.of(2020, 1, 31).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2022, 1, 31).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P2Y"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateYears3() {
		ZonedDateTime date = LocalDate.of(2020, 2, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2022, 2, 28).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P2Y"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateYears4() {
		ZonedDateTime date = LocalDate.of(2018, 2, 28).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2020, 2, 29).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P2Y"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateMonth1() {
		ZonedDateTime date = LocalDate.of(2018, 2, 28).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 4, 30).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P2M"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateMonth2() {
		ZonedDateTime date = LocalDate.of(2018, 2, 28).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 31).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P3M"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateMonth3() {
		ZonedDateTime date = LocalDate.of(2018, 5, 3).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 8, 3).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P3M"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateDay1() {
		ZonedDateTime date = LocalDate.of(2018, 5, 3).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 6).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P3D"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateDay2() {
		ZonedDateTime date = LocalDate.of(2018, 5, 3).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 13).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P10D"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateDay3() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 6, 8).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P10D"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateWeek1() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 6, 5).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P7D"));
		assertEquals(shifted.toString(), expected, shifted);
		assertEquals(shifted.toString(), expected.getDayOfWeek(), date.getDayOfWeek());
	}

	@Test
	public void shouldShiftDateWeek2() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 6, 5).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P1W"));
		assertEquals(shifted.toString(), expected, shifted);
		assertEquals(shifted.toString(), expected.getDayOfWeek(), date.getDayOfWeek());
	}

	@Test
	public void shouldShiftDateWeek3() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 6, 12).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P2W"));
		assertEquals(shifted.toString(), expected, shifted);
		assertEquals(shifted.toString(), expected.getDayOfWeek(), date.getDayOfWeek());
	}

	@Test
	public void shouldShiftDateWeek4() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atStartOfDay(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 6, 12).atStartOfDay(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("P14D"));
		assertEquals(shifted.toString(), expected, shifted);
		assertEquals(shifted.toString(), expected.getDayOfWeek(), date.getDayOfWeek());
	}

	@Test
	public void shouldShiftDateHour1() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(10, 0)).atZone(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(18, 0)).atZone(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("PT8H"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateHour2() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(22, 0)).atZone(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 30).atTime(LocalTime.of(6, 0)).atZone(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("PT8H"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateHour3() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(22, 0)).atZone(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 30).atTime(LocalTime.of(22, 0)).atZone(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("PT24H"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateMinute1() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(22, 0)).atZone(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(22, 30)).atZone(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("PT30M"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldShiftDateMinute2() {
		ZonedDateTime date = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(22, 45)).atZone(systemDefault());
		ZonedDateTime expected = LocalDate.of(2018, 5, 29).atTime(LocalTime.of(23, 0)).atZone(systemDefault());
		ZonedDateTime shifted = shiftDate(date, PeriodDuration.parse("PT15M"));
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldPlusMonthsNormal() {
		int expectedDay = 4;

		ZonedDateTime date = LocalDate.of(2001, Month.JANUARY, expectedDay).atStartOfDay(systemDefault());
		assertEquals(expectedDay, date.getDayOfMonth());

		// increase by single months
		for (int i = 0; i < 5; i++) {
			date = shiftMonths(date, 1);
			assertEquals(expectedDay, date.getDayOfMonth());
		}

		// also when increase by multiple months
		for (int i = 0; i < 5; i++) {
			date = shiftMonths(date, 4);
			assertEquals(expectedDay, date.getDayOfMonth());
		}
	}

	@Test
	public void shouldPlusMonthsLast() {

		ZonedDateTime date;

		// increase by single months
		date = LocalDate.of(2020, 1, 31).atStartOfDay(systemDefault());

		// february
		date = shiftMonths(date, 1);
		assertEquals(date.toString(), Month.FEBRUARY, date.getMonth());
		assertEquals(date.toString(), Month.FEBRUARY.length(true), date.getDayOfMonth());

		// march
		date = shiftMonths(date, 1);
		assertEquals(date.toString(), Month.MARCH, date.getMonth());
		assertEquals(date.toString(), Month.MARCH.length(true), date.getDayOfMonth());

		// april
		date = shiftMonths(date, 1);
		assertEquals(date.toString(), Month.APRIL, date.getMonth());
		assertEquals(date.toString(), Month.APRIL.length(true), date.getDayOfMonth());

		// may
		date = shiftMonths(date, 1);
		assertEquals(date.toString(), Month.MAY, date.getMonth());
		assertEquals(date.toString(), Month.MAY.length(true), date.getDayOfMonth());

		// also when increase by multiple months
		date = LocalDate.of(2020, 1, 31).atStartOfDay(systemDefault());

		// march
		date = shiftMonths(date, 2);
		assertEquals(date.toString(), Month.MARCH, date.getMonth());
		assertEquals(date.toString(), Month.MARCH.length(true), date.getDayOfMonth());

		// june
		date = shiftMonths(date, 3);
		assertEquals(date.toString(), Month.JUNE, date.getMonth());
		assertEquals(date.toString(), Month.JUNE.length(true), date.getDayOfMonth());

	}

	@Test
	public void shouldCalcShiftDays1() {
		ZonedDateTime past = ZonedDateTime.now().minusDays(35);
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P1M");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		assertTrue(shiftedDate.isBefore(now));
		assertTrue(shiftedDate.toString(), shiftedDate.isAfter(now.minusMonths(2)));
	}

	@Test
	public void shouldCalcShiftDays2() {
		ZonedDateTime past = LocalDate.of(2007, 12, 3).atTime(LocalTime.now()).atZone(systemDefault());
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P1M");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);
		assertEquals(shiftedDate.toString(), 3, shiftedDate.getDayOfMonth());
		assertTrue(shiftedDate.toString(), shiftedDate.isBefore(now));
		assertTrue(shiftedDate.toString(), shiftedDate.isAfter(now.minusMonths(2)));
	}

	@Test
	public void shouldCalcShiftDays3() {
		ZonedDateTime past = ZonedDateTime.now().minusDays(20);
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P7D");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		assertTrue(shiftedDate.toString(), shiftedDate.isAfter(now.minusDays(9)));
		assertTrue(shiftedDate.isBefore(now));
		assertEquals(past.getDayOfWeek(), shiftedDate.getDayOfWeek());
	}

	@Test
	public void shouldCalcShiftDays4() {
		ZonedDateTime past = ZonedDateTime
				.parse("2007-12-03T10:15:30+01:00", ISO_OFFSET_DATE_TIME.withZone(systemDefault()));
		ZonedDateTime now = ZonedDateTime.now();
		PeriodDuration periodDuration = PeriodDuration.parse("P7D");
		ZonedDateTime shiftedDate = shiftByMultipleOfPeriod(past, now, periodDuration);

		// since we are many years and months before now, and a year is 356 days and a month is 28 days, we inexact, but at least we must be a more than P7D before now
		assertTrue(shiftedDate.toString(), shiftedDate.isBefore(now));
	}

	@Test
	public void shouldCalcShift7Days1Fixed() {
		ZonedDateTime date1 = LocalDate.of(2017, 10, 30).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2020, 1, 2).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P7D"));
		assertEquals(date1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2019, 12, 30).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShift7Days2Fixed() {
		ZonedDateTime date1 = LocalDate.of(2017, 10, 30).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2021, 4, 28).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P7D"));
		assertEquals(date1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 26).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShift7Days3Fixed() {
		ZonedDateTime date1 = LocalDate.of(2021, 4, 21).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2021, 4, 28).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P7D"));
		assertEquals(date1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 21).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShift1Days1Fixed() {
		ZonedDateTime date1 = LocalDate.of(2017, 10, 30).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2020, 1, 2).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P1D"));

		ZonedDateTime expected = LocalDate.of(2020, 1, 1).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShift1Days2Fixed() {
		ZonedDateTime date1 = LocalDate.of(2017, 10, 30).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2021, 4, 28).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P1D"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShift1Days3Fixed() {
		ZonedDateTime date1 = LocalDate.of(2021, 4, 21).atStartOfDay(systemDefault());
		ZonedDateTime date2 = LocalDate.of(2021, 4, 28).atStartOfDay(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(date1, date2, PeriodDuration.parse("P1D"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftYears1Fixed() {
		ZonedDateTime d1 = LocalDate.of(2007, 6, 3).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 6, 3).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1Y"));

		ZonedDateTime expected = LocalDate.of(2019, 6, 3).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftYears2Fixed() {
		ZonedDateTime d1 = LocalDate.of(2007, 6, 3).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 6, 3).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P2Y"));

		ZonedDateTime expected = LocalDate.of(2019, 6, 3).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths1Fixed() {
		ZonedDateTime d1 = LocalDate.of(2007, 6, 3).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 6, 3).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 5, 3).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths2Fixed() {
		ZonedDateTime d1 = LocalDate.of(2007, 6, 3).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 7, 3).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 6, 3).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths3Fixed() {
		ZonedDateTime d1 = LocalDate.of(2007, 6, 3).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 6, 2).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 5, 3).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths4Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 31).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 7, 30).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 6, 30).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths5Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 2, 28).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 7, 30).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 6, 30).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths6Fixed() {
		ZonedDateTime d1 = LocalDate.of(2020, 2, 29).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 7, 30).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1M"));

		ZonedDateTime expected = LocalDate.of(2020, 6, 30).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftMonths7Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 31).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2020, 7, 30).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P3M"));

		ZonedDateTime expected = LocalDate.of(2020, 4, 30).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftWeek1Fixed() {
		ZonedDateTime d1 = LocalDate.of(2021, 4, 6).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 28).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1W"));
		assertEquals(d1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftWeek2Fixed() {
		ZonedDateTime d1 = LocalDate.of(2021, 4, 6).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1W"));
		assertEquals(d1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 20).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftWeek3Fixed() {
		ZonedDateTime d1 = LocalDate.of(2021, 4, 24).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P7D"));
		assertEquals(d1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 24).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftWeek4Fixed() {
		ZonedDateTime d1 = LocalDate.of(2019, 8, 13).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P2W"));
		assertEquals(d1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 20).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftWeek5Fixed() {
		ZonedDateTime d1 = LocalDate.of(2021, 4, 24).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P14D"));
		assertEquals(d1.getDayOfWeek(), shifted.getDayOfWeek());

		ZonedDateTime expected = LocalDate.of(2021, 4, 24).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftDay1Fixed() {
		ZonedDateTime d1 = LocalDate.of(2021, 4, 24).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1D"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 26).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftDay2Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 5).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P1D"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 26).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftDay3Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 5).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P75D"));

		ZonedDateTime expected = LocalDate.of(2021, 2, 13).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftDay4Fixed() {
		ZonedDateTime d1 = LocalDate.of(2018, 1, 5).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 26).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("P75D"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 19).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftHours1Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 5).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atStartOfDay(ZoneId.systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("PT8H"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 25).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}

	@Test
	public void shouldCalcShiftHours2Fixed() {
		ZonedDateTime d1 = LocalDate.of(2017, 1, 10).atStartOfDay(ZoneId.systemDefault());
		ZonedDateTime d2 = LocalDate.of(2021, 4, 27).atTime(LocalTime.of(12, 47)).atZone(systemDefault());

		ZonedDateTime shifted = shiftByMultipleOfPeriod(d1, d2, PeriodDuration.parse("PT8H"));

		ZonedDateTime expected = LocalDate.of(2021, 4, 25).atStartOfDay(ZoneId.systemDefault());
		assertEquals(shifted.toString(), expected, shifted);
	}
}
