package li.strolch.utils.time;

import static li.strolch.utils.I18nUtils.i18n;

import java.time.Duration;
import java.time.Period;
import java.time.temporal.ChronoUnit;
import java.util.Locale;

public class PeriodDurationFormatter {

	public static String formatPeriodDuration(Locale locale, PeriodDuration periodDuration) {

		StringBuilder sb = new StringBuilder();
		int u;

		u = (int) periodDuration.get(ChronoUnit.YEARS);
		if (u > 0) {
			sb.append(u).append(" ").append(i18n(locale, "years"));
			periodDuration = periodDuration.minus(Period.ofYears(u));

			if (periodDuration.isZero())
				return sb.toString();
			sb.append(" ");
		}

		u = (int) periodDuration.get(ChronoUnit.MONTHS);
		if (u > 0) {
			sb.append(u).append(" ").append(i18n(locale, "months"));
			periodDuration = periodDuration.minus(Period.ofMonths(u));

			if (periodDuration.isZero())
				return sb.toString();
			sb.append(" ");
		}

		u = (int) periodDuration.get(ChronoUnit.DAYS);
		if (u > 0) {
			if (u % 7 == 0)
				sb.append(u / 7).append(" ").append(i18n(locale, "weeks"));
			else
				sb.append(u).append(" ").append(i18n(locale, "days"));
			periodDuration = periodDuration.minus(Period.ofDays(u));

			if (periodDuration.isZero())
				return sb.toString();
			sb.append(" ");
		}

		Duration duration = periodDuration.getDuration();
		if (duration.isZero())
			return sb.toString();

		u = (int) duration.toHours();
		if (u > 0) {
			sb.append(u).append(" ").append(i18n(locale, "hours"));
			duration = duration.minusHours(u);

			if (duration.isZero())
				return sb.toString();
			sb.append(" ");
		}

		u = (int) duration.toMinutes();
		if (u > 0) {
			sb.append(u).append(" ").append(i18n(locale, "minutes"));
			duration = duration.minusMinutes(u);

			if (duration.isZero())
				return sb.toString();
			sb.append(" ");
		}

		u = (int) duration.toMillis() / 1000;
		sb.append(u).append(" ").append(i18n(locale, "seconds"));

		return sb.toString();
	}
}
