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

import li.strolch.utils.helper.MathHelper;

/**
 * Default factory for date formats used for serialization.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
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
	public String formatDate(Date date) {
		return getDateFormat().format(date);
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

	@Override
	public Date parseDate(String date) {
		return getDateFormat().parse(date);
	}

	@Override
	public long parseDuration(String duration) {
		return getDurationFormat().parse(duration);
	}

	@Override
	public long parseWorktime(String worktime) {
		return getDurationFormat().parse(worktime);
	}
}
