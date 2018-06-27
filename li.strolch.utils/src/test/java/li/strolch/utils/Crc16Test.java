package li.strolch.utils;

import static org.junit.Assert.assertArrayEquals;

import org.junit.Test;

public class Crc16Test {

	@Test
	public void shouldCrc16_0() {
		byte[] data = { 0x12, 0x34, 0x56, 0x78, (byte) 0x90 };
		byte[] expected = { 0x48, (byte) 0xE6 };
		assertArrayEquals(expected, Crc.crcCcitt(data));
	}

	@Test
	public void shouldCrc16_1() {
		byte[] data = { (byte) 0x80, 0x01, 0x00, 0x01, 0x02, 0x0D, 0x01 };
		byte[] expected = { (byte) 0x90, (byte) 0xF0 };
		assertArrayEquals(expected, Crc.crcCcitt(data));
	}

	@Test
	public void shouldCrc16_2() {
		byte[] data = { (byte) 0x80, 0x11, 0x00, 0x07, 0x04, 0x09, 0x09, 0x01, 0x01 };
		byte[] expected = { (byte) 0xCA, (byte) 0x64 };
		assertArrayEquals(expected, Crc.crcCcitt(data));
	}

	@Test
	public void shouldCrc16_3() {
		byte[] data = { (byte) 0x80, 0x00, 0x00, 0x03, 0x00 };
		byte[] expected = { 0x77, (byte) 0x83 };
		assertArrayEquals(expected, Crc.crcCcitt(data));
	}

	@Test
	public void shouldCrc16_4() {
		byte[] data = { 0x20, (byte) 0x80, 0x00, 0x00, 0x03, 0x00, 0x30 };
		byte[] expected = { 0x77, (byte) 0x83 };
		assertArrayEquals(expected, Crc.crcCcitt(data, 1, data.length - 1));
	}
}
