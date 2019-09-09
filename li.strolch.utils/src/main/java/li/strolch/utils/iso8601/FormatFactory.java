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

import java.util.Date;

/**
 * This interface defines methods for formatting values for UI representation and also defines factory methods for
 * formatters for parsing and formatting duration and date values
 *
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 */
public interface FormatFactory {

	/**
	 * return the formatter for dates
	 *
	 * @return {@link DateFormat}
	 */
	public DateFormat getDateFormat();

	/**
	 * the date format used in xml import and export
	 *
	 * @return {@link DateFormat}
	 */
	public DateFormat getXmlDateFormat();

	/**
	 * Formats a date using {@link #getDateFormat()}
	 *
	 * @param date
	 * 		the date to format to string
	 *
	 * @return String representation of the date
	 */
	public String formatDate(Date date);

	/**
	 * Formats a long as date using {@link #getDateFormat()}
	 *
	 * @param date
	 * 		the date to format to string
	 *
	 * @return String representation of the date
	 */
	public String formatDate(long date);

	/**
	 * Formats a floating point number to have the configured number of decimals
	 *
	 * @param value
	 * 		the value to format
	 *
	 * @return the floating point formatted as a string
	 */
	public String formatFloat(double value);

	/**
	 * Parses a date using {@link #getDateFormat()}
	 *
	 * @param date
	 * 		the string to parse to date
	 *
	 * @return the date
	 */
	public Date parseDate(String date);
}