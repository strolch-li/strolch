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

import java.util.HashMap;
import java.util.Map;

import li.strolch.utils.helper.BaseEncoding;

/**
 * Simple helper class to generate the reverse alphabets for {@link BaseEncoding}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
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

		Map<Byte, Byte> valueToIndex = new HashMap<>();
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
