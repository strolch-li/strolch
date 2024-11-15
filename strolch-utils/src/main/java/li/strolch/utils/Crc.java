/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import static li.strolch.utils.helper.ByteHelper.toByteArrayBigEndian;

/**
 * <p>Implements Cyclic Redundancy Checks</p>
 *
 * <p>Fast byte-wise CRC16 calculation, using the code from Christian d'Heureuse, Inventec Informatik AG, Switzerland,
 * www.source-code.biz</p>
 */
public class Crc {

	private static final short[] crcTable;

	static {
		int poly = 0x1021;
		crcTable = new short[256];
		for (int x = 0; x < 256; x++) {
			int w = x << 8;
			for (int i = 0; i < 8; i++) {
				if ((w & 0x8000) != 0) {
					w = (w << 1) ^ poly;
				} else {
					w <<= 1;
				}
			}
			crcTable[x] = (short) w;
		}
	}

	/**
	 * Implements a CRC-CCITT (XModem), using the initial value 0x0000 and the polynomial 0x1021
	 *
	 * @param bytes the bytes to CRC
	 *
	 * @return the 2 byte CRC
	 *
	 * @see <a href="https://www.lammertbies.nl/comm/info/crc-calculation.html">crc-calculation</a>
	 */
	public static byte[] crcCcitt(byte[] bytes) {
		return toByteArrayBigEndian(crcCcittBytes(bytes));
	}

	/**
	 * Implements a CRC-CCITT (XModem), using the initial value 0x0000 and the polynomial 0x1021
	 *
	 * @param bytes  the bytes to CRC
	 * @param src    start if array to do CRC
	 * @param length end in array to do CRC
	 *
	 * @return the 2 byte CRC
	 *
	 * @see <a href="https://www.lammertbies.nl/comm/info/crc-calculation.html">crc-calculation</a>
	 */
	public static byte[] crcCcitt(byte[] bytes, int src, int length) {
		return toByteArrayBigEndian(crcCcittBytes(bytes, src, length));
	}

	/**
	 * Implements a CRC-CCITT (XModem), using the initial value 0x0000 and the polynomial 0x1021
	 *
	 * @param bytes the bytes to CRC
	 *
	 * @return the CRC as short
	 *
	 * @see <a href="https://www.lammertbies.nl/comm/info/crc-calculation.html">crc-calculation</a>
	 */
	public static short crcCcittBytes(byte[] bytes) {
		return crcCcittBytes(bytes, 0, bytes.length);
	}

	/**
	 * Implements a CRC-CCITT (XModem), using the initial value 0x0000 and the polynomial 0x1021
	 *
	 * @param bytes  the bytes to CRC
	 * @param src    start if array to do CRC
	 * @param length end in array to do CRC
	 *
	 * @return the CRC as short
	 *
	 * @see <a href="https://www.lammertbies.nl/comm/info/crc-calculation.html">crc-calculation</a>
	 */
	public static short crcCcittBytes(byte[] bytes, int src, int length) {
		int crc = 0x0000;
		for (int i = src; i < length; i++) {
			byte aData = bytes[i];
			crc = ((crc << 8) & 0xFF00) ^ (crcTable[(crc >> 8) ^ (aData & 0xFF)] & 0xFFFF);
		}
		return (short) crc;
	}
}
