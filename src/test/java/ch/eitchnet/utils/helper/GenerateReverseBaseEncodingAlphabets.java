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

import java.util.HashMap;
import java.util.Map;

/**
 * Simple helper class to generate the reverse alphabets for {@link BaseDecoding}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class GenerateReverseBaseEncodingAlphabets {

	public static void main(String[] args) {

		System.out.println(generateReverseAlphabet("REV_BASE_16", BaseEncoding.BASE_16));
		System.out.println(generateReverseAlphabet("REV_BASE_32", BaseEncoding.BASE_32));
		System.out.println(generateReverseAlphabet("REV_BASE_32_CROCKFORD", BaseEncoding.BASE_32_CROCKFORD));
		System.out.println(generateReverseAlphabet("REV_BASE_32_DMEDIA", BaseEncoding.BASE_32_DMEDIA));
		System.out.println(generateReverseAlphabet("REV_BASE_32_HEX", BaseEncoding.BASE_32_HEX));
		System.out.println(generateReverseAlphabet("REV_BASE_64", BaseEncoding.BASE_64));
		System.out.println(generateReverseAlphabet("REV_BASE_64_SAFE", BaseEncoding.BASE_64_SAFE));
	}

	public static String generateReverseAlphabet(String name, byte[] alphabet) {

		Map<Byte, Byte> valueToIndex = new HashMap<Byte, Byte>();
		for (byte i = 0; i < alphabet.length; i++) {
			Byte value = Byte.valueOf(i);
			Byte key = Byte.valueOf(alphabet[value]);
			if (valueToIndex.containsKey(key))
				throw new RuntimeException("Alphabet hast twice the same value " + key + " at index " + value);
			valueToIndex.put(key, value);
		}

		StringBuilder sb = new StringBuilder();
		sb.append("private static final byte[] " + name + " = { ");

		Byte minusOne = Byte.valueOf((byte) -1);
		for (int i = 0; i < 128; i++) {
			Byte index = Byte.valueOf((byte) i);
			Byte value = valueToIndex.get(index);
			if (value == null)
				sb.append(minusOne.toString());
			else
				sb.append(value.toString());

			if (i < 127)
				sb.append(", ");
		}

		sb.append(" };");

		return sb.toString();
	}
}
