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

/**
 * interface for the worktime format
 *
 * @author Martin Smock &lt;smock.martin@gmail.com&gt;
 */
public interface WorktimeFormat {

	/**
	 * format a long to string
	 *
	 * @param l
	 * 		the work time
	 *
	 * @return formatted string if the long argument
	 */
	public String format(long l);

	/**
	 * parse a string to long
	 *
	 * @param s
	 * 		the string to parse
	 *
	 * @return the long value parsed
	 */
	public long parse(String s);

}
