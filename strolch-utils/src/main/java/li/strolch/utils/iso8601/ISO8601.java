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

import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import static java.time.ZoneId.systemDefault;
import static java.time.temporal.ChronoField.*;

/**
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 */
@SuppressWarnings("nls")
public class ISO8601 implements DateFormat {

	private static final Logger logger = LoggerFactory.getLogger(ISO8601.class);

	public static final Date EMPTY_VALUE = parseToDate("-");
	public static final LocalTime MAX_LOCAL_TIME = LocalTime.MAX.truncatedTo(ChronoUnit.MILLIS);
	public static final ZonedDateTime YEAR_3000 = LocalDate.of(3000, 1, 1).atStartOfDay(systemDefault());

	public static final ZonedDateTime EMPTY_VALUE_ZONED_DATE = ZonedDateTime.ofInstant(Instant.EPOCH, systemDefault());

	public static final LocalDateTime EMPTY_VALUE_LOCAL_DATE = LocalDateTime.ofInstant(Instant.EPOCH, systemDefault());

	private static final DateTimeFormatter _LOCAL_DATE_TIME_SECONDS;

	static {
		_LOCAL_DATE_TIME_SECONDS = new DateTimeFormatterBuilder() //
				.parseCaseInsensitive() //
				.appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD) //
				.appendLiteral('-') //
				.appendValue(MONTH_OF_YEAR, 2) //
				.appendLiteral('-') //
				.appendValue(DAY_OF_MONTH, 2) //
				.appendLiteral('T') //
				.appendValue(HOUR_OF_DAY, 2) //
				.appendLiteral(':') //
				.appendValue(MINUTE_OF_HOUR, 2) //
				.optionalStart() //
				.appendLiteral(':') //
				.appendValue(SECOND_OF_MINUTE, 2) //
				.toFormatter();
	}

	public static final DateTimeFormatter OFFSET_DATE_TIME_SECONDS;

	static {
		OFFSET_DATE_TIME_SECONDS = new DateTimeFormatterBuilder() //
				.append(_LOCAL_DATE_TIME_SECONDS) //
				.parseLenient() //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter() //
				.withZone(ZoneId.systemDefault());
	}

	public static final DateTimeFormatter OFFSET_DATE_TIME_MILLIS;

	static {
		OFFSET_DATE_TIME_MILLIS = new DateTimeFormatterBuilder() //
				.append(_LOCAL_DATE_TIME_SECONDS) //
				.optionalStart() //
				.parseLenient() //
				.appendFraction(NANO_OF_SECOND, 0, 3, true) //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter() //
				.withZone(ZoneId.systemDefault());
	}

	public static final DateTimeFormatter OFFSET_DATE_TIME_MICROS;

	static {
		OFFSET_DATE_TIME_MICROS = new DateTimeFormatterBuilder() //
				.append(_LOCAL_DATE_TIME_SECONDS) //
				.optionalStart() //
				.parseLenient() //
				.appendFraction(NANO_OF_SECOND, 0, 6, true) //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter() //
				.withZone(ZoneId.systemDefault());
	}

	public static final DateTimeFormatter OFFSET_DATE_TIME_NANOS;

	static {
		OFFSET_DATE_TIME_NANOS = new DateTimeFormatterBuilder() //
				.append(_LOCAL_DATE_TIME_SECONDS) //
				.optionalStart() //
				.parseLenient() //
				.appendFraction(NANO_OF_SECOND, 0, 9, true) //
				.appendOffsetId() //
				.parseStrict() //
				.toFormatter() //
				.withZone(ZoneId.systemDefault());
	}

	@Override
	public String format(long timePoint) {
		if (timePoint == Long.MAX_VALUE || timePoint == Long.MIN_VALUE || timePoint == 0L)
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

	public static Date parseToDate(String s) {
		return Date.from(parseToZdt(s).toInstant());
	}

	public static ZonedDateTime parseToZdt(String s) {
		return _parse(s, NANO_OF_SECOND);
	}

	public static ZonedDateTime parseToZdt(String s, ChronoField precision) {
		return _parse(s, precision);
	}

	private static ZonedDateTime _parse(String s, ChronoField precision) {
		if (StringHelper.isEmpty(s)) {
			String msg = "An empty value can not pe parsed to a date!";
			throw new IllegalArgumentException(msg);
		}

		if (s.equals("-"))
			return ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC);
		return ZonedDateTime.parse(s, getIso8601Formatter(precision));
	}

	public static String toString(long timeMs) {
		return toString(Instant.ofEpochMilli(timeMs).atZone(systemDefault()));
	}

	public static String toString(LocalDateTime localDateTime) {
		return toString(localDateTime.atZone(systemDefault()));
	}

	public static String toString(ZonedDateTime zonedDateTime) {
		return toString(zonedDateTime, MILLI_OF_SECOND);
	}

	public static String toString(ZonedDateTime zonedDateTime, ChronoField precision) {
		return getIso8601Formatter(precision).format(zonedDateTime);
	}

	public static String toString(Date date) {
		return getIso8601Formatter(MILLI_OF_SECOND).format(date.toInstant());
	}

	public static DateTimeFormatter getIso8601Formatter(ChronoField precision) {
		return switch (precision) {
			case SECOND_OF_MINUTE -> OFFSET_DATE_TIME_SECONDS;
			case MILLI_OF_SECOND -> OFFSET_DATE_TIME_MILLIS;
			case MICRO_OF_SECOND -> OFFSET_DATE_TIME_MICROS;
			case NANO_OF_SECOND -> OFFSET_DATE_TIME_NANOS;
			default -> throw new IllegalArgumentException("Unsupported precision " + precision);
		};
	}
}
