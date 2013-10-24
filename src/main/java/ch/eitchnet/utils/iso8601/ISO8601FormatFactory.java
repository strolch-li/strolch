package ch.eitchnet.utils.iso8601;

import ch.eitchnet.utils.helper.MathHelper;

/**
 * Default factory for date formats used for serialization.
 * 
 * @author msmock
 */
public class ISO8601FormatFactory implements FormatFactory {

	private static ISO8601FormatFactory instance = new ISO8601FormatFactory();

	/**
	 * the singleton constructor
	 */
	private ISO8601FormatFactory() {
		// singleton 
	}

	/**
	 * @return the instance
	 */
	public static ISO8601FormatFactory getInstance() {
		return instance;
	}

	@Override
	public ISO8601 getDateFormat() {
		return new ISO8601();
	}

	@Override
	public ISO8601Duration getDurationFormat() {
		return new ISO8601Duration();
	}

	@Override
	public ISO8601Worktime getWorktimeFormat() {
		return new ISO8601Worktime();
	}

	@Override
	public ISO8601 getXmlDateFormat() {
		return new ISO8601();
	}

	@Override
	public ISO8601Duration getXmlDurationFormat() {
		return new ISO8601Duration();
	}

	@Override
	public String formatDate(long date) {
		return getDateFormat().format(date);
	}

	@Override
	public String formatDuration(long duration) {
		return getDurationFormat().format(duration);
	}

	@Override
	public String formatWorktime(long worktime) {
		return getDurationFormat().format(worktime);
	}

	@Override
	public String formatFloat(double value) {
		return Double.toString(MathHelper.toPrecision(value));
	}
}
