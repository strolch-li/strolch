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

import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
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
		return ZonedDateTime.parse(s, DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()));
	}

	public static Date parseToDate(String s) {
		return Date.from(parseToZdt(s).toInstant());
	}

	public static String toString(ZonedDateTime zonedDateTime) {
		return DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()).format(zonedDateTime);
	}

	public static String toString(Date date) {
		return DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()).format(date.toInstant());
	}

	public static void main(String[] args) {
		System.out.println(toString(ISO8601.parseToZdt("-")));
	}
}
