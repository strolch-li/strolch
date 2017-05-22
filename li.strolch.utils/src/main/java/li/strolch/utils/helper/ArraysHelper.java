/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.utils.helper;

import java.util.Arrays;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ArraysHelper {

	/**
	 * Returns true if the byte array contains the given byte value
	 * 
	 * @param bytes
	 *            the array to search in
	 * @param searchByte
	 *            the value to search for
	 * 
	 * @return true if found, false if not
	 */
	public static boolean contains(byte[] bytes, byte searchByte) {
		for (byte b : bytes) {
			if (b == searchByte)
				return true;
		}
		return false;
	}

	/**
	 * Creates a simple copy of the given array
	 * 
	 * @param bytes
	 *            the array to copy
	 * 
	 * @return the copy
	 */
	public static byte[] copyOf(byte[] bytes) {
		return Arrays.copyOf(bytes, bytes.length);
	}

	/**
	 * Creates a simple copy of the given array
	 * 
	 * @param bytes
	 *            the array to copy
	 * 
	 * @return the copy
	 */
	public static char[] copyOf(char[] chars) {
		return Arrays.copyOf(chars, chars.length);
	}
}
