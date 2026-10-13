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

package li.strolch.utils.helper;

import li.strolch.utils.helper.BinUtils.ENDIANESS;
import org.junit.Test;

import java.security.InvalidParameterException;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

public class BinUtilsTest {

	@Test
	public void testBitTest_ValidPositions() {
		byte b1 = (byte) 0b10000000;
		assertTrue(BinUtils.bitTest(b1, 0));
		for (int i = 1; i <= 7; i++) {
			assertFalse(BinUtils.bitTest(b1, i));
		}

		byte b2 = (byte) 0b00000001;
		assertTrue(BinUtils.bitTest(b2, 7));
		for (int i = 0; i <= 6; i++) {
			assertFalse(BinUtils.bitTest(b2, i));
		}

		byte b3 = (byte) 0b01010101;
		assertFalse(BinUtils.bitTest(b3, 0));
		assertTrue(BinUtils.bitTest(b3, 1));
		assertFalse(BinUtils.bitTest(b3, 2));
		assertTrue(BinUtils.bitTest(b3, 3));
		assertFalse(BinUtils.bitTest(b3, 4));
		assertTrue(BinUtils.bitTest(b3, 5));
		assertFalse(BinUtils.bitTest(b3, 6));
		assertTrue(BinUtils.bitTest(b3, 7));

		byte b4 = (byte) 0b10101010;
		assertTrue(BinUtils.bitTest(b4, 0));
		assertFalse(BinUtils.bitTest(b4, 1));
		assertTrue(BinUtils.bitTest(b4, 2));
		assertFalse(BinUtils.bitTest(b4, 3));
		assertTrue(BinUtils.bitTest(b4, 4));
		assertFalse(BinUtils.bitTest(b4, 5));
		assertTrue(BinUtils.bitTest(b4, 6));
		assertFalse(BinUtils.bitTest(b4, 7));

		byte bZero = 0x00;
		for (int i = 0; i <= 7; i++) {
			assertFalse(BinUtils.bitTest(bZero, i));
		}

		byte bAll = (byte) 0xFF;
		for (int i = 0; i <= 7; i++) {
			assertTrue(BinUtils.bitTest(bAll, i));
		}
	}

	@Test
	public void testBitTest_InvalidPositions_ThrowsInvalidParameterException() {
		assertThrows(InvalidParameterException.class, () -> BinUtils.bitTest((byte) 0, -1));
		assertThrows(InvalidParameterException.class, () -> BinUtils.bitTest((byte) 0, 8));
		assertThrows(InvalidParameterException.class, () -> BinUtils.bitTest((byte) 0, 100));
	}

	@Test
	public void testByteToBin() {
		assertEquals("00000000", BinUtils.byteToBin((byte) 0x00));
		assertEquals("11111111", BinUtils.byteToBin((byte) 0xFF));
		assertEquals("10000000", BinUtils.byteToBin((byte) 0x80));
		assertEquals("00000001", BinUtils.byteToBin((byte) 0x01));
		assertEquals("10101010", BinUtils.byteToBin((byte) 0xAA));
		assertEquals("01010101", BinUtils.byteToBin((byte) 0x55));

		for (int i = 0; i < 256; i++) {
			byte b = (byte) i;
			String bin = BinUtils.byteToBin(b);
			assertEquals(8, bin.length());
			for (int bit = 0; bit < 8; bit++) {
				char expected = BinUtils.bitTest(b, bit) ? '1' : '0';
				assertEquals(expected, bin.charAt(bit));
			}
		}
	}

	@Test
	public void testBytesToBin_BigEndian() {
		assertEquals("", BinUtils.bytesToBin(new byte[0], ENDIANESS.BIG_ENDIAN));
		assertEquals("00000001", BinUtils.bytesToBin(new byte[]{0x01}, ENDIANESS.BIG_ENDIAN));
		assertEquals("00000001 00000010", BinUtils.bytesToBin(new byte[]{0x01, 0x02}, ENDIANESS.BIG_ENDIAN));
		assertEquals("10101010 01010101 11111111",
				BinUtils.bytesToBin(new byte[]{(byte) 0xAA, 0x55, (byte) 0xFF}, ENDIANESS.BIG_ENDIAN));
	}

	@Test
	public void testBytesToBin_LittleEndian() {
		assertEquals("", BinUtils.bytesToBin(new byte[0], ENDIANESS.LITTLE_ENDIAN));
		assertEquals("00000001", BinUtils.bytesToBin(new byte[]{0x01}, ENDIANESS.LITTLE_ENDIAN));
		assertEquals("00000010 00000001", BinUtils.bytesToBin(new byte[]{0x01, 0x02}, ENDIANESS.LITTLE_ENDIAN));
		assertEquals("11111111 01010101 10101010",
				BinUtils.bytesToBin(new byte[]{(byte) 0xAA, 0x55, (byte) 0xFF}, ENDIANESS.LITTLE_ENDIAN));
	}

	@Test
	public void testBinToBytes_BigEndian() {
		assertArrayEquals(new byte[0], BinUtils.binToBytes(""));
		assertArrayEquals(new byte[]{0x00}, BinUtils.binToBytes("   "));
		assertArrayEquals(new byte[]{0x01}, BinUtils.binToBytes("00000001"));
		assertArrayEquals(new byte[]{0x01, 0x02}, BinUtils.binToBytes("00000001 00000010"));
		assertArrayEquals(new byte[]{0x01, 0x02}, BinUtils.binToBytes("0000000100000010"));
		assertArrayEquals(new byte[]{(byte) 0xAA, 0x55, (byte) 0xFF},
				BinUtils.binToBytes("10101010 01010101 11111111"));

		// Default overload should use BIG_ENDIAN
		assertArrayEquals(BinUtils.binToBytes("00000001 00000010", ENDIANESS.BIG_ENDIAN),
				BinUtils.binToBytes("00000001 00000010"));
	}

	@Test
	public void testBinToBytes_LittleEndian() {
		assertArrayEquals(new byte[0], BinUtils.binToBytes("", ENDIANESS.LITTLE_ENDIAN));
		assertArrayEquals(new byte[]{0x01}, BinUtils.binToBytes("00000001", ENDIANESS.LITTLE_ENDIAN));
		assertArrayEquals(new byte[]{0x02, 0x01},
				BinUtils.binToBytes("00000001 00000010", ENDIANESS.LITTLE_ENDIAN));
		assertArrayEquals(new byte[]{(byte) 0xFF, 0x55, (byte) 0xAA},
				BinUtils.binToBytes("10101010 01010101 11111111", ENDIANESS.LITTLE_ENDIAN));
	}

	@Test
	public void testBinToBytes_PartialBytes() {
		assertArrayEquals(new byte[]{0x01}, BinUtils.binToBytes("1"));
		assertArrayEquals(new byte[]{0x0A}, BinUtils.binToBytes("1010"));
		assertArrayEquals(new byte[]{0x01, 0x02}, BinUtils.binToBytes("1 00000010"));
	}

	@Test
	public void testBinToBytes_InvalidChars_ThrowsInvalidParameterException() {
		assertThrows(InvalidParameterException.class, () -> BinUtils.binToBytes("00102001"));
		assertThrows(InvalidParameterException.class, () -> BinUtils.binToBytes("abc"));
		assertThrows(InvalidParameterException.class, () -> BinUtils.binToBytes("0101 0102"));
		assertThrows(InvalidParameterException.class, () -> BinUtils.binToBytes("0101-0101"));
	}

	@Test
	public void testEndianessEnum() {
		assertEquals(ENDIANESS.BIG_ENDIAN, ENDIANESS.valueOf("BIG_ENDIAN"));
		assertEquals(ENDIANESS.LITTLE_ENDIAN, ENDIANESS.valueOf("LITTLE_ENDIAN"));
		assertEquals(2, ENDIANESS.values().length);
		assertNotNull(ENDIANESS.values()[0]);
		assertNotNull(ENDIANESS.values()[1]);
	}

	@Test
	public void testRoundtrip_BigEndian() {
		for (int len = 0; len <= 16; len++) {
			byte[] original = new byte[len];
			for (int i = 0; i < len; i++) {
				original[i] = (byte) ((i * 37 + 13) & 0xFF);
			}
			String bin = BinUtils.bytesToBin(original, ENDIANESS.BIG_ENDIAN);
			byte[] decoded = BinUtils.binToBytes(bin, ENDIANESS.BIG_ENDIAN);
			assertArrayEquals(original, decoded);
		}
	}

	@Test
	public void testRoundtrip_LittleEndian() {
		for (int len = 0; len <= 16; len++) {
			byte[] original = new byte[len];
			for (int i = 0; i < len; i++) {
				original[i] = (byte) ((i * 37 + 13) & 0xFF);
			}
			String bin = BinUtils.bytesToBin(original, ENDIANESS.LITTLE_ENDIAN);
			byte[] decoded = BinUtils.binToBytes(bin, ENDIANESS.LITTLE_ENDIAN);
			assertArrayEquals(original, decoded);
		}
	}
}
