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

import static java.time.ZoneId.systemDefault;
import static java.time.temporal.ChronoField.*;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.util.Date;

import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 */
@SuppressWarnings("nls")
public class ISO8601 implements DateFormat {

	private static final Logger logger = LoggerFactory.getLogger(ISO8601.class);

	public static final Date EMPTY_VALUE = parseToDate("-");
	public static final ZonedDateTime EMPTY_VALUE_ZONED_DATE = ZonedDateTime
			.ofInstant(EMPTY_VALUE.toInstant(), systemDefault());

	public static final DateTimeFormatter ISO_LOCAL_DATE;

	static {
		ISO_LOCAL_DATE = new DateTimeFormatterBuilder().appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD) //
				.appendLiteral('-') //
				.appendValue(MONTH_OF_YEAR, 2) //
				.appendLiteral('-') //
				.appendValue(DAY_OF_MONTH, 2) //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_LOCAL_TIME;

	static {
		ISO_LOCAL_TIME = new DateTimeFormatterBuilder() //
				.appendValue(HOUR_OF_DAY, 2) //
				.appendLiteral(':') //
				.appendValue(MINUTE_OF_HOUR, 2) //
				.optionalStart() //
				.appendLiteral(':') //
				.appendValue(SECOND_OF_MINUTE, 2) //
				.optionalStart() //
				.appendFraction(NANO_OF_SECOND, 0, 3, true) //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_LOCAL_TIME_NO_FRACTIONS;

	static {
		ISO_LOCAL_TIME_NO_FRACTIONS = new DateTimeFormatterBuilder() //
				.appendValue(HOUR_OF_DAY, 2) //
				.appendLiteral(':') //
				.appendValue(MINUTE_OF_HOUR, 2) //
				.optionalStart() //
				.appendLiteral(':') //
				.appendValue(SECOND_OF_MINUTE, 2) //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_LOCAL_DATE_TIME;

	static {
		ISO_LOCAL_DATE_TIME = new DateTimeFormatterBuilder() //
				.parseCaseInsensitive() //
				.append(ISO_LOCAL_DATE) //
				.appendLiteral('T') //
				.append(ISO_LOCAL_TIME) //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_LOCAL_DATE_TIME_NO_FRACTIONS;

	static {
		ISO_LOCAL_DATE_TIME_NO_FRACTIONS = new DateTimeFormatterBuilder() //
				.parseCaseInsensitive() //
				.append(ISO_LOCAL_DATE) //
				.appendLiteral('T') //
				.append(ISO_LOCAL_TIME_NO_FRACTIONS) //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_OFFSET_DATE_TIME;

	static {
		ISO_OFFSET_DATE_TIME = new DateTimeFormatterBuilder() //
				.parseCaseInsensitive() //
				.append(ISO_LOCAL_DATE_TIME) //
				.parseLenient() //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter();
	}

	public static final DateTimeFormatter ISO_OFFSET_DATE_TIME_ZONE;

	static {
		ISO_OFFSET_DATE_TIME_ZONE = ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault());
	}

	public static final DateTimeFormatter ISO_OFFSET_DATE_TIME_ZONE_NO_FRACTIONS;

	static {
		ISO_OFFSET_DATE_TIME_ZONE_NO_FRACTIONS = new DateTimeFormatterBuilder() //
				.parseCaseInsensitive() //
				.append(ISO_LOCAL_DATE_TIME_NO_FRACTIONS) //
				.parseLenient() //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter();
	}

	@Override
	public String format(long timePoint) {
		if (timePoint == Long.MAX_VALUE || timePoint == Long.MIN_VALUE)
			return "-";

		try {
			Date date = new Date();
			date.setTime(timePoint);
			return format(date);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public String format(Date date) {
		return toString(date);
	}

	@Override
	public long parseLong(String s) {
		return parse(s).getTime();
	}

	@Override
	public Date parse(String s) {
		return parseToDate(s);
	}

	public static ZonedDateTime parseToZdt(String s) {
		if (StringHelper.isEmpty(s)) {
			String msg = "An empty value can not pe parsed to a date!";
			throw new IllegalArgumentException(msg);
		}

		if (s.equals("-"))
			return ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC);
		return ZonedDateTime.parse(s, getIso8601Formatter(false));
	}

	public static Date parseToDate(String s) {
		return Date.from(parseToZdt(s).toInstant());
	}

	public static String toString(LocalDateTime localDateTime) {
		return toString(localDateTime.atZone(systemDefault()));
	}

	public static String toString(ZonedDateTime zonedDateTime) {
		return getIso8601Formatter(false).format(zonedDateTime);
	}

	public static String toString(Date date) {
		return getIso8601Formatter(false).format(date.toInstant());
	}

	public static DateTimeFormatter getIso8601Formatter(boolean noFractions) {
		if (noFractions)
			return ISO_OFFSET_DATE_TIME_ZONE_NO_FRACTIONS;
		return ISO_OFFSET_DATE_TIME_ZONE;
	}

	public static void main(String[] args) {
		System.out.println(toString(ISO8601.parseToZdt("-")));
		System.out.println(toString(ZonedDateTime.now()));
		System.out.println(toString(LocalDateTime.now()));
	}
}
