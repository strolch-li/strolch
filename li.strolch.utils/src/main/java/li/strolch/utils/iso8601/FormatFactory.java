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
	 * @return {@link DurationFormat}
	 */
	public DateFormat getDateFormat();

	/**
	 * return the formatter for durations
	 * 
	 * @return {@link DurationFormat}
	 */
	public DurationFormat getDurationFormat();

	/**
	 * return the formatter for work time
	 * 
	 * @return {@link WorktimeFormat}
	 */
	public WorktimeFormat getWorktimeFormat();

	/**
	 * the date format used in xml import and export
	 * 
	 * @return {@link DateFormat}
	 */
	public DateFormat getXmlDateFormat();

	/**
	 * the duration format used in xml import and export
	 * 
	 * @return {@link DurationFormat}
	 */
	public DurationFormat getXmlDurationFormat();

	/**
	 * Formats a date using {@link #getDateFormat()}
	 * 
	 * @param date
	 *            the date to format to string
	 * 
	 * @return String representation of the date
	 */
	public String formatDate(Date date);

	/**
	 * Formats a long as date using {@link #getDateFormat()}
	 * 
	 * @param date
	 *            the date to format to string
	 * 
	 * @return String representation of the date
	 */
	public String formatDate(long date);

	/**
	 * Formats a duration using {@link #getDateFormat()}
	 * 
	 * @param duration
	 *            the duration to format to string
	 * 
	 * @return String representation of the duration
	 */
	public String formatDuration(long duration);

	/**
	 * Formats a work time duration using {@link #getDateFormat()}
	 * 
	 * @param worktime
	 *            the work time duration to format to string
	 * 
	 * @return String representation of the work time duration
	 */
	public String formatWorktime(long worktime);

	/**
	 * Formats a floating point number to have the configured number of decimals
	 * 
	 * @param value
	 *            the value to format
	 * 
	 * @return the floating point formatted as a string
	 */
	public String formatFloat(double value);

	/**
	 * Parses a date using {@link #getDateFormat()}
	 * 
	 * @param date
	 *            the string to parse to date
	 * 
	 * @return the date
	 */
	public Date parseDate(String date);

	/**
	 * Parses a duration using {@link #getDateFormat()}
	 * 
	 * @param duration
	 *            the string to parse to duration
	 * 
	 * @return the duration
	 */
	public long parseDuration(String duration);

	/**
	 * Parses a work time duration using {@link #getDateFormat()}
	 * 
	 * @param worktime
	 *            the string duration to parse to work time
	 * 
	 * @return the work time
	 */
	public long parseWorktime(String worktime);
}