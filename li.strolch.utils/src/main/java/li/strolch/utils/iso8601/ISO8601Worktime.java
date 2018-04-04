/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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
package li.strolch.utils.iso8601;

/**
 * <p>
 * Duration is defined as a duration of time, as specified in ISO 8601, Section 5.5.3.2. Its lexical representation is
 * the ISO 8601 extended format: <b>PnYnMnDnTnHnMnS</b>
 * </p>
 * <ul>
 * <li>The "P" (period) is required</li>
 * <li>"n" represents a positive number</li>
 * <li>years is (Y)</li>
 * <li>months is (M)</li>
 * <li>days is (D)</li>
 * <li>time separator is (T), required if any lower terms are given</li>
 * <li>hours is (H)</li>
 * <li>minutes is (M)</li>
 * <li>seconds is (S)</li>
 * </ul>
 * <p>
 * An optional preceding minus sign ("-") is also allowed to indicate a negative duration. If the sign is omitted then a
 * positive duration is assumed. For example: <b>&lt;an_element duration="PT2H5M2.37S" /&gt;</b> is a 2 hour, 5 minute,
 * and 2.37 second duration
 * </p>
 * <p>
 * <b>Remark:</b> since a duration of a day may be measured in hours may vary from 23 an 25 a duration day unit doesn't
 * have a meaning, if we do not know either the start or the end, we restrict ourself to measure a duration in hours,
 * minutes and seconds
 * </p>
 *
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 * @author Michael Gatto &lt;michael@gatto.ch&gt; (reimplementation using enum)
 */
@SuppressWarnings("nls")
public class ISO8601Worktime implements WorktimeFormat {

	/**
	 * The time representations available, as enum, with the associated millis.
	 *
	 * @author gattom
	 */
	public enum TimeDuration {

		SECOND(1000, 'S'),
		MINUTE(60 * SECOND.duration(), 'M'),
		HOUR(60 * MINUTE.duration(), 'H');

		final long millis;
		final char isoChar;

		TimeDuration(long milli, char isorep) {
			this.millis = milli;
			this.isoChar = isorep;
		}

		public long duration() {
			return this.millis;
		}

		public static TimeDuration getTimeDurationFor(String isostring, int unitIndex) {
			char duration = isostring.charAt(unitIndex);
			switch (duration) {
			case 'S':
				if (isostring.substring(0, unitIndex).contains("T"))
					return SECOND;
				throw new NumberFormatException(
						duration + " is not a valid unit of time in ISO8601 without a preceeding T (e.g.: PT1S)");
			case 'H':
				if (isostring.substring(0, unitIndex).contains("T"))
					return HOUR;
				throw new NumberFormatException(
						duration + " is not a valid unit of time in ISO8601 without a preceeding T (e.g.: PT1H)");
			case 'M':
				return MINUTE;
			default:
				throw new NumberFormatException(duration + " is not a valid unit of time in ISO8601");
			}
		}

	}

	/**
	 * check if c is a number char including the decimal decimal dot (.)
	 *
	 * @param c
	 * 		the character to check
	 *
	 * @return boolean return true if the given char is a number or a decimal dot (.), false otherwise
	 */
	private static boolean isNumber(char c) {
		boolean isNumber = Character.isDigit(c) || (c == '.');
		return isNumber;
	}

	/**
	 * Parses the given string to a pseudo ISO 8601 duration
	 *
	 * @param s
	 * 		the string to be parsed to a duration which must be coded as a ISO8601 value
	 *
	 * @return long the time value which represents the duration
	 */
	@Override
	public long parse(String s) {

		long newResult = 0;

		// throw exception, if the string is not of length > 2
		if (s.length() < 3)
			throw new NumberFormatException(s + " cannot be parsed to ISA 8601 Duration");

		char p = s.charAt(0);

		if (p == 'P') {
			int newposition = 1;
			do {
				if (s.charAt(newposition) == 'T') {
					// skip the separator specifying where the time starts.
					newposition++;
				}
				// read the string representing the numeric value
				String val = parseNumber(newposition, s);
				double numVal = Double.parseDouble(val);
				newposition += val.length();
				// get the time unit
				TimeDuration unit = TimeDuration.getTimeDurationFor(s, newposition);
				// skip the time duration character
				newposition++;
				// increment the value.
				newResult += unit.duration() * numVal;
			} while (newposition < s.length());

			return newResult;
		}

		throw new NumberFormatException(s + " cannot be parsed to ISO 8601 Duration");
	}

	/**
	 * Return the substring of s starting at index i (in s) that contains a numeric string.
	 *
	 * @param index
	 * 		The start index in string s
	 * @param s
	 * 		The string to analyze
	 *
	 * @return the substring containing the numeric portion of s starting at index i.
	 */
	private String parseNumber(int index, String s) {
		int i = index;
		int start = i;
		while (i < s.length()) {
			if (!isNumber(s.charAt(i)))
				break;
			i++;
		}
		String substring = s.substring(start, i);
		return substring;
	}

	/**
	 * Format the given time duration unit into the string buffer. This function displays the given duration in units of
	 * the given unit, and returns the remainder.
	 * <p>
	 * Thus, a duration of 86401000 (one day and one second) will add the representation of one day if unit is DAY (1D)
	 * and return 1000 as the remainder with respect of this unit. If the given unit is HOUR, then this function adds
	 * 24H to the {@link StringBuilder}, and returns 1000 as the remainder.
	 *
	 * @param sb
	 * 		The {@link StringBuilder} to add the given duration with the right unit
	 * @param duration
	 * 		The duration to add
	 * @param unit
	 * 		The unit of this duration
	 *
	 * @return The remainder of the given duration, modulo the time unit.
	 */
	private long formatTimeDuration(StringBuilder sb, long duration, TimeDuration unit) {

		long remainder = duration;

		if (unit.equals(TimeDuration.SECOND) || remainder >= unit.duration()) {

			long quantity = remainder / unit.duration();
			remainder = remainder % unit.duration();
			sb.append(quantity);

			if (unit.equals(TimeDuration.SECOND)) {

				long millis = remainder;
				if (millis == 0) {
					// to not have the decimal point
				} else if (millis > 99) {
					sb.append("." + millis);
				} else if (millis > 9) {
					sb.append(".0" + millis);
				} else {
					sb.append(".00" + millis);
				}
			}

			sb.append(unit.isoChar);
		}
		return remainder;
	}

	/**
	 * Formats the given time duration to a pseudo ISO 8601 duration string
	 *
	 * @param duration
	 * 		the duration
	 *
	 * @return String the duration formatted as a ISO8601 duration string
	 */
	@Override
	public String format(long duration) {

		if (duration == 0)
			return "PT0S";

		StringBuilder sb = new StringBuilder();
		sb.append('P');
		sb.append('T');
		long remainder = formatTimeDuration(sb, duration, TimeDuration.HOUR);
		remainder = formatTimeDuration(sb, remainder, TimeDuration.MINUTE);
		remainder = formatTimeDuration(sb, remainder, TimeDuration.SECOND);

		return sb.toString();
	}

}
