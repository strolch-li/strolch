package li.strolch.utils.helper;

import java.security.InvalidParameterException;
import java.util.Arrays;

public class HexUtils {

	private static final char[] hexArray = "0123456789abcdef".toCharArray();
	private static final int MAX_ARRAY_SIZE = Math.toIntExact((Integer.MAX_VALUE - 1L) * 8L / 25L); //687194766;

	public static String bytesToHex(byte[] byteArray) {
		if (byteArray.length > MAX_ARRAY_SIZE)
			throw new InvalidParameterException("Size of byte array too large for hex conversion: " + byteArray.length);

		int spaces = byteArray.length / 8;
		char[] hexChars = new char[byteArray.length * 3 + spaces];
		int pos = 0;
		int high;
		int low;
		int v;
		for (int i = 0; i < byteArray.length; i++) {
			v = byteArray[i] & 0xFF;
			high = v >>> 4;
			low = v & 0x0F;
			hexChars[pos] = hexArray[high];
			pos++;
			hexChars[pos] = hexArray[low];
			pos++;
			hexChars[pos] = ' ';
			pos++;
			if (i % 8==7) {
				hexChars[pos] = ' ';
				pos++;
			}
		}
		return new String(hexChars).trim();
	}

	public static String bytesToHex2(byte[] byteArray) {
		if (byteArray.length > MAX_ARRAY_SIZE)
			throw new InvalidParameterException("Size of byte array too large for hex conversion: " + byteArray.length);

		int spaces = byteArray.length / 8;
		char[] hexChars = new char[byteArray.length * 3 + spaces];
		int pos = 0;
		int high;
		int low;
		int v;
		for (int i = 0; i < byteArray.length; i++) {
			v = byteArray[i] & 0xFF;
			high = v >>> 4;
			low = v & 0x0F;
			hexChars[pos] = hexArray[high];
			pos++;
			hexChars[pos] = hexArray[low];
			pos++;
			hexChars[pos] = ' ';
			pos++;
			if (i % 8==7) {
				hexChars[pos] = ' ';
				pos++;
			}
		}
//		return Arrays.toString(hexChars);
		return new String(hexChars).trim();
	}

	public static byte[] hexToBytes(String prettyHex) {
		int cpt = 0;
		char[] chars = prettyHex.toCharArray();
		char[] charsnospace = new char[chars.length];

		for (int i = 0; i < chars.length; i++) {
			if (chars[i] == ' ') {
				cpt++;
			}
			else {
				charsnospace[i - cpt] = chars[i];
			}
		}

		int len = chars.length - cpt;
		if (len % 2 != 0) {
			throw new IllegalArgumentException("Hex string must have an even length");
		}

		// Create a byte array that is half the length of the hex string
		byte[] data = new byte[len / 2];

		// Convert each pair of hex characters to a byte
		int highNibble;
		int lowNibble;
		for (int i = 0; i < len; i += 2) {
			highNibble = Character.digit(charsnospace[i], 16);
			lowNibble = Character.digit(charsnospace[i + 1], 16);

			if (highNibble == -1 || lowNibble == -1) {
				throw new IllegalArgumentException("Invalid hex digit");
			}

			data[i / 2] = (byte) ((highNibble << 4) + lowNibble);
		}

		return data;
	}

}
