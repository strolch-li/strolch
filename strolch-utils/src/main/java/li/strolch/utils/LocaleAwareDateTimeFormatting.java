/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import li.strolch.utils.collections.MapOfMaps;

import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Locale;

import static java.time.format.DateTimeFormatter.*;
import static java.time.temporal.ChronoField.HOUR_OF_DAY;
import static java.time.temporal.ChronoField.MINUTE_OF_HOUR;
import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;

public class LocaleAwareDateTimeFormatting {

	private static final MapOfMaps<Locale, DateFormattingHint, DateTimeFormatter> formatters = synchronizedMapOfMaps(
			new MapOfMaps<>());

	public synchronized static DateTimeFormatter getFormatter(Locale locale, DateFormattingHint hint) {
		return formatters.computeIfAbsent(locale, hint, () -> evaluateDateTimeFormatter(locale, hint));
	}

	private static DateTimeFormatter evaluateDateTimeFormatter(Locale locale, DateFormattingHint hint) {
		return switch (hint) {
			case None -> ISO_LOCAL_DATE_TIME;
			case Date, DateDashIfEmpty -> ISO_LOCAL_DATE;
			case DateTime, DateTimeDashIfEmpty -> getDateTimeFormatter(locale);
			case DateTimeSeconds, DateTimeSecondsDashIfEmpty -> getDateTimeSecondsFormatter(locale);
			case Time -> getTimeFormatter(locale);
			case TimeSeconds -> ISO_LOCAL_TIME;
		};
	}

	private static DateTimeFormatter getDateTimeFormatter(Locale locale) {
		return new DateTimeFormatterBuilder()
				.parseCaseInsensitive()
				.append(ISO_LOCAL_DATE)
				.appendLiteral(' ')
				.append(new DateTimeFormatterBuilder()
						.appendValue(HOUR_OF_DAY, 2)
						.appendLiteral(':')
						.appendValue(MINUTE_OF_HOUR, 2)
						.toFormatter(locale))
				.toFormatter(locale);
	}

	private static DateTimeFormatter getTimeFormatter(Locale locale) {
		return new DateTimeFormatterBuilder()
				.appendValue(HOUR_OF_DAY, 2)
				.appendLiteral(':')
				.appendValue(MINUTE_OF_HOUR, 2)
				.toFormatter(locale);
	}

	private static DateTimeFormatter getDateTimeSecondsFormatter(Locale locale) {
		return new DateTimeFormatterBuilder()
				.parseCaseInsensitive()
				.append(ISO_LOCAL_DATE)
				.appendLiteral(' ')
				.append(ISO_LOCAL_TIME)
				.toFormatter(locale);
	}
}
