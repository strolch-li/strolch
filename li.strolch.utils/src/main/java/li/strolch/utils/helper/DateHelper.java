package li.strolch.utils.helper;

import java.time.LocalDateTime;
import java.time.chrono.Chronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.ResourceBundle;

import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * Helper class to format dates and periods to Strings
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class DateHelper {

	/**
	 * Formats the given ISO 8601 date to the given locale using {@link FormatStyle#MEDIUM}. If the year is > 2100 then
	 * a - (dash) is returned.
	 * 
	 * @param locale
	 *            the locale to use
	 * @param isoDate
	 *            the date as ISO String
	 * @param withTimeIfNonZero
	 *            if true and the time part is not 0, then it is appended to the string, if the time is not 0, then it
	 *            is always appended
	 * @return the string in the locale' format using {@link FormatStyle#MEDIUM}
	 */
	public static String formatDate(Locale locale, String isoDate, boolean withTimeIfNonZero) {
		LocalDateTime parse = LocalDateTime.parse(isoDate, DateTimeFormatter.ISO_DATE_TIME);

		if (parse.getYear() > 2100)
			return "-";

		DateTimeFormatter formatter;
		if (withTimeIfNonZero && (parse.getHour() != 0 || parse.getMinute() != 0)) {
			String pattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(FormatStyle.MEDIUM,
					FormatStyle.MEDIUM, Chronology.ofLocale(locale), locale);
			formatter = DateTimeFormatter.ofPattern(pattern);
		} else {
			String pattern = DateTimeFormatterBuilder.getLocalizedDateTimePattern(FormatStyle.MEDIUM, null,
					Chronology.ofLocale(locale), locale);
			formatter = DateTimeFormatter.ofPattern(pattern);
		}

		return parse.format(formatter);
	}

	/**
	 * Formats the given period in the form Pn[DWM] (n days, weeks or months) to the written out form of: &lt;prefix&gt; n
	 * day(s) / n week(s) / n month(s). Note that the bundle must contain the following keys:
	 * <ul>
	 * <li>days</li>
	 * <li>day</li>
	 * <li>weeks</li>
	 * <li>week</li>
	 * <li>months</li>
	 * <li>month</li>
	 * </ul>
	 * 
	 * @param prefixKey
	 *            if not null, then prefix lookup key in bundle to set before result
	 * @param bundle
	 *            the bundle where to get the translations
	 * @param iso8601Period
	 *            the period
	 * @return the formatted period
	 */
	public static String formatPeriod(ResourceBundle bundle, String prefixKey, String iso8601Period) {

		char selectedPeriodUnit = iso8601Period.charAt(iso8601Period.length() - 1);
		int selectedPeriodNumber = Integer.parseInt(iso8601Period.substring(1, iso8601Period.length() - 1));
		String labelString;
		if (prefixKey == null) {
			labelString = selectedPeriodNumber + " ";
		} else {
			labelString = bundle.getString(prefixKey) + " " + selectedPeriodNumber + " ";
		}

		switch (selectedPeriodUnit) {
		case 'D':
			labelString = labelString + (selectedPeriodNumber > 1 ? bundle.getString("days") : bundle.getString("day"));
			break;
		case 'W':
			labelString = labelString
					+ (selectedPeriodNumber > 1 ? bundle.getString("weeks") : bundle.getString("week"));
			break;
		case 'M':
			labelString = labelString
					+ (selectedPeriodNumber > 1 ? bundle.getString("months") : bundle.getString("month"));
			break;
		}

		return labelString;
	}

	/**
	 * Parses the given ISO8601 time stamp and truncates the time from it, returning the time in long
	 * 
	 * @param iso8601Timestamp
	 *            the ISO 8601 date to parse
	 * 
	 * @return the truncated time in milliseconds
	 */
	public static long truncateTimeFromTimestamp(String iso8601Timestamp) {

		Date dateToCut = ISO8601FormatFactory.getInstance().parseDate(iso8601Timestamp);

		Calendar cal = Calendar.getInstance();
		cal.setTime(dateToCut);
		cal.clear(Calendar.HOUR);
		cal.clear(Calendar.HOUR_OF_DAY);
		cal.clear(Calendar.MINUTE);
		cal.clear(Calendar.SECOND);
		cal.clear(Calendar.MILLISECOND);
		cal.clear(Calendar.AM_PM);

		return cal.getTimeInMillis();
	}
}
