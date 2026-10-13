/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.Test;

import java.nio.charset.StandardCharsets;
import java.security.InvalidParameterException;

import static li.strolch.utils.helper.ByteHelper.toByteArrayBigEndian;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

public class CrcTest {

	@Test
	public void testCrcCcitt_EmptyArray() {
		byte[] empty = new byte[0];
		assertArrayEquals(new byte[]{0x00, 0x00}, Crc.crcCcitt(empty));
		assertEquals((short) 0x0000, Crc.crcCcittBytes(empty));
		assertArrayEquals(new byte[]{0x00, 0x00}, Crc.crcCcitt(empty, 0, 0));
		assertEquals((short) 0x0000, Crc.crcCcittBytes(empty, 0, 0));
	}

	@Test
	public void testCrcCcitt_SingleByte() {
		byte[] singleZero = {0x00};
		assertArrayEquals(new byte[]{0x00, 0x00}, Crc.crcCcitt(singleZero));
		assertEquals((short) 0x0000, Crc.crcCcittBytes(singleZero));

		byte[] singleOne = {0x01};
		assertArrayEquals(new byte[]{0x10, 0x21}, Crc.crcCcitt(singleOne));
		assertEquals((short) 0x1021, Crc.crcCcittBytes(singleOne));
	}

	@Test
	public void testCrcCcitt_KnownVectors() {
		byte[] data0 = {0x12, 0x34, 0x56, 0x78, (byte) 0x90};
		byte[] expected0 = {0x48, (byte) 0xE6};
		assertArrayEquals(expected0, Crc.crcCcitt(data0));
		assertEquals((short) 0x48E6, Crc.crcCcittBytes(data0));

		byte[] data1 = {(byte) 0x80, 0x01, 0x00, 0x01, 0x02, 0x0D, 0x01};
		byte[] expected1 = {(byte) 0x90, (byte) 0xF0};
		assertArrayEquals(expected1, Crc.crcCcitt(data1));
		assertEquals((short) 0x90F0, Crc.crcCcittBytes(data1));

		byte[] data2 = {(byte) 0x80, 0x11, 0x00, 0x07, 0x04, 0x09, 0x09, 0x01, 0x01};
		byte[] expected2 = {(byte) 0xCA, (byte) 0x64};
		assertArrayEquals(expected2, Crc.crcCcitt(data2));
		assertEquals((short) 0xCA64, Crc.crcCcittBytes(data2));

		byte[] data3 = {(byte) 0x80, 0x00, 0x00, 0x03, 0x00};
		byte[] expected3 = {0x77, (byte) 0x83};
		assertArrayEquals(expected3, Crc.crcCcitt(data3));
		assertEquals((short) 0x7783, Crc.crcCcittBytes(data3));

		byte[] standardAscii123456789 = "123456789".getBytes(StandardCharsets.US_ASCII);
		byte[] expectedAscii = {0x31, (byte) 0xC3};
		assertArrayEquals(expectedAscii, Crc.crcCcitt(standardAscii123456789));
		assertEquals((short) 0x31C3, Crc.crcCcittBytes(standardAscii123456789));
	}

	@Test
	public void testCrcCcitt_WithOffsetAndLength() {
		byte[] data = {0x20, (byte) 0x80, 0x00, 0x00, 0x03, 0x00, 0x30};
		byte[] expected = {0x77, (byte) 0x83};

		assertArrayEquals(expected, Crc.crcCcitt(data, 1, 5));
		assertEquals((short) 0x7783, Crc.crcCcittBytes(data, 1, 5));

		byte[] subData = {(byte) 0x80, 0x00, 0x00, 0x03, 0x00};
		assertArrayEquals(Crc.crcCcitt(subData), Crc.crcCcitt(data, 1, 5));
		assertEquals(Crc.crcCcittBytes(subData), Crc.crcCcittBytes(data, 1, 5));

		assertArrayEquals(Crc.crcCcitt(data), Crc.crcCcitt(data, 0, data.length));
		assertEquals(Crc.crcCcittBytes(data), Crc.crcCcittBytes(data, 0, data.length));

		assertArrayEquals(new byte[]{0x00, 0x00}, Crc.crcCcitt(data, 2, 0));
		assertEquals((short) 0x0000, Crc.crcCcittBytes(data, 2, 0));
	}

	@Test
	public void testCrcCcitt_NegativeSrc_ThrowsInvalidParameterException() {
		byte[] data = {0x01, 0x02, 0x03};
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcitt(data, -1, 2));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcittBytes(data, -1, 2));
	}

	@Test
	public void testCrcCcitt_LengthExceedsBounds_ThrowsInvalidParameterException() {
		byte[] data = {0x01, 0x02, 0x03};
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcitt(data, 0, 4));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcittBytes(data, 0, 4));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcitt(data, 2, 2));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcittBytes(data, 2, 2));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcitt(data, 4, 0));
		assertThrows(InvalidParameterException.class, () -> Crc.crcCcittBytes(data, 4, 0));
	}

	@Test
	public void testCrcCcitt_ConsistencyBetweenByteArrayAndShort() {
		byte[] data = new byte[256];
		for (int i = 0; i < data.length; i++) {
			data[i] = (byte) i;
		}

		short crcShort = Crc.crcCcittBytes(data);
		byte[] crcBytes = Crc.crcCcitt(data);
		assertArrayEquals(toByteArrayBigEndian(crcShort), crcBytes);

		for (int offset = 0; offset < 20; offset++) {
			for (int length = 0; length < 20; length++) {
				short sliceShort = Crc.crcCcittBytes(data, offset, length);
				byte[] sliceBytes = Crc.crcCcitt(data, offset, length);
				assertArrayEquals(toByteArrayBigEndian(sliceShort), sliceBytes);
			}
		}
	}
}
