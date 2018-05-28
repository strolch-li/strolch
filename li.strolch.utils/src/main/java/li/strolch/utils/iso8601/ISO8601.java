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
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

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
	public String format(Date date) {
		return DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()).format(date.toInstant());
	}

	/**
	 * added by msmock convert a long to ISO8601
	 *
	 * @param timePoint
	 * 		the timepoint
	 *
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
			return format(date);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			return null;
		}
	}

	@Override
	public long parseLong(String s) {
		return parse(s).getTime();
	}

	public static void main(String[] args) {
		Date d = new Date();
		long start = System.currentTimeMillis();
		for (int i = 0; i < 10000000; i++) {
			String s = new ISO8601().format(d);
			Date d1 = new ISO8601().parse(s);
			if (!d.equals(d1))
				throw new IllegalStateException("Dates not same: " + d + " / " + d1);
		}
		System.out.println("Took " + (System.currentTimeMillis() - start));
	}

	/**
	 * parse ISO8601 date to long
	 *
	 * @param s
	 * 		the string to parse
	 *
	 * @return time point as long
	 *
	 * @throws IllegalArgumentException
	 * 		if the string can not be parsed
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

		ZonedDateTime zd = ZonedDateTime
				.parse(s, DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneId.systemDefault()));
		return Date.from(zd.toInstant());
	}
}
