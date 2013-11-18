package ch.eitchnet.utils.iso8601;

import java.text.DecimalFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

/**
 * 
 */
@SuppressWarnings("nls")
public class ISO8601 implements DateFormat {

	private static final Logger logger = LoggerFactory.getLogger(ISO8601.class);

	//misc. numeric formats used in formatting
	private DecimalFormat xxFormat = new DecimalFormat("00");
	private DecimalFormat xxxFormat = new DecimalFormat("000");
	private DecimalFormat xxxxFormat = new DecimalFormat("0000");

	/**
     * 
     */
	private Calendar parseToCalendar(String text) {

		// check optional leading sign
		char sign;
		int start;
		if (text.startsWith("-")) {
			sign = '-';
			start = 1;
		} else if (text.startsWith("+")) {
			sign = '+';
			start = 1;
		} else {
			sign = '+'; // no sign specified, implied '+'
			start = 0;
		}

		/**
		 * format of the string is: YYYY-MM-DDThh:mm:ss.SSSTZD
		 */
		int year, month, day, hour, min, sec, millisec;
		String timeZone;
		try {

			// year (YYYY)
			year = Integer.parseInt(text.substring(start, start + 4));
			start += 4;
			// delimiter '-'
			if (text.charAt(start) != '-') {
				return null;
			}
			start++;

			// month (MM)
			month = Integer.parseInt(text.substring(start, start + 2));
			start += 2;
			// delimiter '-'
			if (text.charAt(start) != '-') {
				return null;
			}
			start++;

			// day (DD)
			day = Integer.parseInt(text.substring(start, start + 2));
			start += 2;
			// delimiter 'T'
			if (text.charAt(start) != 'T') {
				return null;
			}
			start++;

			// hour (hh)
			hour = Integer.parseInt(text.substring(start, start + 2));
			start += 2;
			// delimiter ':'
			if (text.charAt(start) != ':') {
				return null;
			}
			start++;

			// minute (mm)
			min = Integer.parseInt(text.substring(start, start + 2));
			start += 2;
			// delimiter ':'
			if (text.charAt(start) != ':') {
				return null;
			}
			start++;

			// second (ss)
			sec = Integer.parseInt(text.substring(start, start + 2));
			start += 2;

			// delimiter '.'
			if (text.charAt(start) == '.') {
				start++;
				// millisecond (SSS)
				millisec = Integer.parseInt(text.substring(start, start + 3));
				start += 3;
			} else {
				millisec = 0;
			}

			if (text.charAt(start) == '+' || text.charAt(start) == '-') {
				timeZone = "GMT" + text.substring(start);
			} else if (text.substring(start).equals("Z")) {
				timeZone = "GMT";
			} else {
				return null;
			}

		} catch (IndexOutOfBoundsException e) {
			return null;
		} catch (NumberFormatException e) {
			return null;
		}

		TimeZone tz = TimeZone.getTimeZone(timeZone);
		if (!tz.getID().equals(timeZone)) {
			// invalid time zone
			return null;
		}

		// create Calendar
		Calendar cal = Calendar.getInstance(tz);
		cal.setLenient(false);

		if (sign == '-' || year == 0) {
			// 
			cal.set(Calendar.YEAR, year + 1);
			cal.set(Calendar.ERA, GregorianCalendar.BC);
		} else {
			cal.set(Calendar.YEAR, year);
			cal.set(Calendar.ERA, GregorianCalendar.AD);
		}

		// 
		cal.set(Calendar.MONTH, month - 1);
		cal.set(Calendar.DAY_OF_MONTH, day);
		cal.set(Calendar.HOUR_OF_DAY, hour);
		cal.set(Calendar.MINUTE, min);
		cal.set(Calendar.SECOND, sec);
		cal.set(Calendar.MILLISECOND, millisec);

		try {
			cal.getTime();
		} catch (IllegalArgumentException e) {
			return null;
		}

		return cal;
	}

	/**
     * 
     */
	private String format(Calendar cal) {

		if (cal == null) {
			throw new IllegalArgumentException("argument can not be null");
		}

		// determine era and adjust year if necessary
		int year = cal.get(Calendar.YEAR);
		if (cal.isSet(Calendar.ERA) && cal.get(Calendar.ERA) == GregorianCalendar.BC) {
			/**
			 * calculate year using astronomical system: year n BCE => astronomical year -n + 1
			 */
			year = 0 - year + 1;
		}

		/**
		 * format of date/time string is: YYYY-MM-DDThh:mm:ss.SSSTZD
		 */
		StringBuilder sWriter = new StringBuilder();
		sWriter.append(this.xxxxFormat.format(year));
		sWriter.append('-');
		sWriter.append(this.xxFormat.format(cal.get(Calendar.MONTH) + 1));
		sWriter.append('-');
		sWriter.append(this.xxFormat.format(cal.get(Calendar.DAY_OF_MONTH)));
		sWriter.append('T');
		sWriter.append(this.xxFormat.format(cal.get(Calendar.HOUR_OF_DAY)));
		sWriter.append(':');
		sWriter.append(this.xxFormat.format(cal.get(Calendar.MINUTE)));
		sWriter.append(':');
		sWriter.append(this.xxFormat.format(cal.get(Calendar.SECOND)));
		sWriter.append('.');
		sWriter.append(this.xxxFormat.format(cal.get(Calendar.MILLISECOND)));
		TimeZone tz = cal.getTimeZone();

		int offset = tz.getOffset(cal.getTimeInMillis());
		if (offset != 0) {
			int hours = Math.abs((offset / (60 * 1000)) / 60);
			int minutes = Math.abs((offset / (60 * 1000)) % 60);
			sWriter.append(offset < 0 ? '-' : '+');
			sWriter.append(this.xxFormat.format(hours));
			sWriter.append(':');
			sWriter.append(this.xxFormat.format(minutes));
		} else {
			sWriter.append('Z');
		}
		return sWriter.toString();
	}

	@Override
	public String format(Date date) {
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		return format(cal);
	}

	/**
	 * added by msmock convert a long to ISO8601
	 * 
	 * @param timePoint
	 * @return time point as ISO8601 String
	 */
	@Override
	public String format(long timePoint) {

		if (timePoint == Long.MAX_VALUE || timePoint == Long.MIN_VALUE) {
			return "-";
		}

		// else
		try {
			Date date = new Date();
			date.setTime(timePoint);
			Calendar cal = Calendar.getInstance();
			cal.setTime(date);
			return format(cal);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public long parseLong(String s) {
		return parse(s).getTime();
	}

	/**
	 * parse ISO8601 date to long
	 * 
	 * @param s
	 *            the string to parse
	 * @return time point as long
	 * @throws NumberFormatException
	 */
	@Override
	public Date parse(String s) {

		if (StringHelper.isEmpty(s)) {
			String msg = "An empty value can not pe parsed to a date!";
			throw new IllegalArgumentException(msg);
		}

		if (s.equals("-")) {
			Calendar cal = Calendar.getInstance();
			cal.clear();
			cal.setTimeZone(TimeZone.getTimeZone("GMT0"));
			return cal.getTime();
		}

		Calendar cal = parseToCalendar(s);
		if (cal != null) {
			return cal.getTime();
		}

		String msg = "Input string " + s + " cannot be parsed to date.";
		throw new IllegalArgumentException(msg);
	}
}
