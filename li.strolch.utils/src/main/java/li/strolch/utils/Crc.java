package li.strolch.utils;

import static li.strolch.utils.helper.ByteHelper.toByteArrayBigEndian;

/**
 * Implements Cyclic Redundancy Checks
 */
public class Crc {

	/**
	 * Implements a CRC-CCITT (XModem), using the initial value 0x0000 and the polynomial 0x1021
	 *
	 * @param bytes
	 * 		the bytes to CRC
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
	 * @param bytes
	 * 		the bytes to CRC
	 *
	 * @return the CRC as short
	 *
	 * @see <a href="https://www.lammertbies.nl/comm/info/crc-calculation.html">crc-calculation</a>
	 */
	public static short crcCcittBytes(byte[] bytes) {
		int crc = 0x0000;          // initial value
		int polynomial = 0x1021;   // 0001 0000 0010 0001  (0, 5, 12)

		for (byte b : bytes) {
			for (int i = 0; i < 8; i++) {
				boolean bit = ((b >> (7 - i) & 1) == 1);
				boolean c15 = ((crc >> 15 & 1) == 1);
				crc <<= 1;
				if (c15 ^ bit)
					crc ^= polynomial;
			}
		}

		crc &= 0xffff;
		return (short) crc;
	}
}
