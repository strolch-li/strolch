package li.strolch.utils.time;

import static java.time.Period.between;

import java.time.Duration;
import java.time.Month;
import java.time.Period;
import java.time.ZonedDateTime;
import java.util.concurrent.TimeUnit;

import li.strolch.utils.dbc.DBC;

public class PeriodHelper {

	public static double daysIn(PeriodDuration periodDuration) {
		return (daysIn(periodDuration.getPeriod()) + (periodDuration.getDuration().toHours() / 24.0));
	}

	public static double daysIn(Period period) {
		return (period.getYears() * 365.0) + (period.getMonths() * 30.0) + period.getDays();
	}

	/**
	 * <p>Special method to add the given number of months to the given date and making sure that the day always stays
	 * the same if possible. I.e. If the given date has the 3. day, then this will also be so on the returned date.</p>
	 * <p>But for the day 28, 29, 30 or 31, if these are the last days of the given month, then the returned month is
	 * also shifted to the last day</p>
	 *
	 * @param date
	 * 		the date to shift
	 * @param nrOfMonths
	 * 		the number of months to shift the given date by
	 *
	 * @return the shifted date
	 */
	public static ZonedDateTime shiftMonths(ZonedDateTime date, long nrOfMonths) {
		int selectedDayOfMonth = date.getDayOfMonth();
		ZonedDateTime next = date.plusMonths(nrOfMonths);
		if (date.toLocalDate().lengthOfMonth() == selectedDayOfMonth)
			return next.withDayOfMonth(next.toLocalDate().lengthOfMonth());
		return next.withDayOfMonth(Math.min(selectedDayOfMonth, next.toLocalDate().lengthOfMonth()));
	}

	/**
	 * Shifts the given date by the given period duration. This method doesn't accept a mixture of period and duration,
	 * one or the other must be zero. Durations may only contain hours and minutes. Periods must only have one part set,
	 * i.e. either year, month or day. Months are handles special in that the shifting is delegated to {@link
	 * #shiftMonths(ZonedDateTime, long)}. Furthermore, this method also specially handles weeks, i.e. if shifting is by
	 * multiple of 7, then this is handled as shifting by weeks
	 *
	 * @param date
	 * 		the date to shift
	 * @param periodDuration
	 * 		the period duration to shift the date by
	 *
	 * @return the shifted date
	 */
	public static ZonedDateTime shiftDate(ZonedDateTime date, PeriodDuration periodDuration) {
		DBC.PRE.assertNotNull("date may not be null!", date);
		DBC.PRE.assertNotNull("periodDuration may not be null!", periodDuration);

		if (periodDuration.isZero())
			return date;

		Duration duration = periodDuration.getDuration();
		Period period = periodDuration.getPeriod();

		if (!period.isZero() && !duration.isZero())
			throw new UnsupportedOperationException(
					"Shifting by Periods and Durations at the same time is not supported!");

		if (!period.isZero()) {

			// shift by years
			if (period.getYears() > 0) {

				if (period.getMonths() != 0 && period.getDays() != 0)
					throw new UnsupportedOperationException(
							"Shifting by multi values not supported: " + periodDuration);

				int numberOfYears = period.getYears();
				ZonedDateTime shifted = date.plusYears(numberOfYears);

				// if the given month was on the last day of february, then keep the last day in the new year as well
				if (date.getMonth() == Month.FEBRUARY && date.getDayOfMonth() == date.toLocalDate().lengthOfMonth())
					shifted = shifted.withDayOfMonth(shifted.toLocalDate().lengthOfMonth());

				return shifted;
			}

			// shift by months
			if (period.getMonths() > 0) {
				if (period.getDays() != 0)
					throw new UnsupportedOperationException(
							"Shifting by multi values not supported: " + periodDuration);

				int numberOfMonths = period.getMonths();
				// use special shift, so we keep end of month if applicable
				return shiftMonths(date, numberOfMonths);
			}

			// shift by weeks
			if (period.getDays() % 7 == 0) {
				int numberOfWeeks = period.getDays() / 7;
				return date.plusWeeks(numberOfWeeks);
			}

			// shift by days
			int numberOfDays = period.getDays();
			return date.plusDays(numberOfDays);
		}

		// shift by hours
		long hours = duration.toHours();
		if (hours > 0) {
			date = date.plusHours(hours);
			duration = duration.minusHours(hours);
		}

		// shift by minutes
		long minutes = duration.toMinutes();
		if (minutes > 0) {
			date = date.plusMinutes(minutes);
			duration = duration.minusMinutes(minutes);
		}

		if (duration.toSecondsPart() > 0 || duration.toMillisPart() > 0)
			throw new UnsupportedOperationException("Only supporting hours and minutes: " + periodDuration);

		return date;
	}

	/**
	 * This special function allows us end shift a start by a multiple of the given {@link PeriodDuration} so that is
	 * before the given end start. It does multiple tries end get as close as possible, due end the inexactness of 30
	 * days being one month, and 365 days being one year.
	 *
	 * @param date
	 * 		the start date to shift before the end
	 * @param end
	 * 		the start before which end stop shifting
	 * @param periodDuration
	 * 		the period shift in multiples by
	 *
	 * @return the shifted start
	 */
	public static ZonedDateTime shiftByMultipleOfPeriod(ZonedDateTime date, ZonedDateTime end,
			PeriodDuration periodDuration) {
		DBC.PRE.assertTrue("date must be before end!", date.isBefore(end));
		DBC.PRE.assertFalse("period duration may not be null!", periodDuration.isZero());

		Duration duration = periodDuration.getDuration();
		Period period = periodDuration.getPeriod();

		if (!period.isZero() && !duration.isZero())
			throw new UnsupportedOperationException(
					"Shifting by Periods and Durations at the same time is not supported!");

		Period between = between(date.toLocalDate(), end.toLocalDate());
		if (between.isZero() || daysIn(between) < 1)
			return date;

		// see if we need end shift by years
		if (period.getYears() > 0 && period.getMonths() == 0 && period.getDays() == 0 && duration.isZero()) {
			int numberOfYears = period.getYears();

			// calculate the number of years to shift
			long yearsBetween = between.getYears();
			long shifts = yearsBetween / numberOfYears;
			long shiftYears = shifts * numberOfYears;
			if (shiftYears == yearsBetween)
				shiftYears -= numberOfYears;

			if (shiftYears < numberOfYears)
				return date;

			return date.plusYears(shiftYears);
		}

		// see if we need end shift by months
		if (period.getMonths() > 0 && period.getYears() == 0 && period.getDays() == 0 && duration.isZero()) {
			int numberOfMonths = period.getMonths();

			// calculate the number of months to shift
			long monthsBetween = between.toTotalMonths();
			// we will accept 30 days as a full month, and increase as well
			if (between.getDays() >= 30)
				monthsBetween += 1;
			long shifts = monthsBetween / numberOfMonths;
			long shiftMonths = shifts * numberOfMonths;
			if (shiftMonths == monthsBetween)
				shiftMonths -= numberOfMonths;

			if (shiftMonths < numberOfMonths)
				return date;

			// use special shift, so we keep end of month if applicable
			return shiftMonths(date, shiftMonths);
		}

		double daysInPeriod = daysIn(periodDuration);

		// see if we need end shift by weeks
		if (period.getDays() != 0 && period.getDays() % 7 == 0 && daysInPeriod % 7 == 0) {
			int numberOfWeeks = period.getDays() / 7;

			// calculate the number of weeks to shift
			long daysInBetween = (long) daysIn(between);
			long weeksBetween = daysInBetween / 7;
			long shifts = weeksBetween / numberOfWeeks;
			long shiftWeeks = shifts * numberOfWeeks;

			if (shiftWeeks < numberOfWeeks)
				return date;

			ZonedDateTime result = date.plusWeeks(shiftWeeks);
			if (!result.isBefore(end))
				return date.plusWeeks(shiftWeeks - 1);
		}

		// see if we are shifting simply by single days
		// this includes 24h durations
		if (daysInPeriod == 1.0)
			return date.plus(between.minusDays(1));

		double daysInBetween = daysIn(between);

		// e.g. period is P70D
		if (daysInPeriod > daysInBetween)
			return date;

		// if shifting by more than one day e.g. P2D or P75D
		if (daysInPeriod > 1.0) {

			long shifts = (long) (daysInBetween / daysInPeriod);
			long shiftDays = (long) (shifts * daysInPeriod);
			if (shiftDays < 1)
				return date;

			return date.plusDays(shiftDays);
		}

		if (!period.isZero())
			throw new IllegalStateException(
					"Expected period to be zero at this point and only duration to be set: " + periodDuration);

		// shifting by e.g. PT8H
		double hoursInPeriod = duration.getSeconds() / 60.0 / 60.0;
		double hoursInBetween = TimeUnit.DAYS.toHours((long) daysInBetween);
		long shifts = (long) (hoursInBetween / hoursInPeriod);
		long shiftHours = (long) (shifts * hoursInPeriod);
		if (shiftHours < 24)
			return date;

		return date.plusDays((shiftHours / 24) - 1);
	}
}
