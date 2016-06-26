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
 * Interface for date formatting
 * 
 * Martin Smock <smock.martin@gmail.com>
 */
public interface DateFormat {

	/**
	 * format a long to string
	 * 
	 * @param timepoint
	 * @return the formatted string of the long value
	 */
	public String format(long timepoint);

	/**
	 * format a Date to string
	 * 
	 * @param date
	 * @return the formatted string of the long value
	 */
	public String format(Date date);

	/**
	 * parse a string to long
	 * 
	 * @param s
	 * @return the value parsed
	 */
	public long parseLong(String s);

	/**
	 * parse a string to Date
	 * 
	 * @param s
	 * @return the value parsed
	 */
	public Date parse(String s);
}
