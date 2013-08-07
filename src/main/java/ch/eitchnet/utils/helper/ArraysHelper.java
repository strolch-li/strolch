/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.helper;

import java.util.Arrays;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
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
}
