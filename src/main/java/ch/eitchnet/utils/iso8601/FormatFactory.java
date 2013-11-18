package ch.eitchnet.utils.iso8601;

import java.util.Date;

/**
 * This interface defines methods for formatting values for UI representation and also defines factory methods for
 * formatters for parsing and formatting duration and date values
 * 
 * @author msmock
 */
public interface FormatFactory {

	/**
	 * return the formatter for dates
	 * 
	 * @return RSPDurationFormat
	 */
	public DateFormat getDateFormat();

	/**
	 * return the formatter for durations
	 * 
	 * @return RSPDurationFormat
	 */
	public DurationFormat getDurationFormat();

	/**
	 * return the formatter for work time
	 * 
	 * @return RSPWorktimeFormat
	 */
	public WorktimeFormat getWorktimeFormat();

	/**
	 * the date format used in xml import and export
	 * 
	 * @return RSPDateFormat
	 */
	public DateFormat getXmlDateFormat();

	/**
	 * the duration format used in xml import and export
	 * 
	 * @return RSPDurationFormat
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
}