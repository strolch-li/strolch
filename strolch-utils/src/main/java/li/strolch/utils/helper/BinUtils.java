package li.strolch.utils.helper;

import java.security.InvalidParameterException;

public class BinUtils {

	public enum ENDIANESS{
		BIG_ENDIAN,	//MSB is stored at Lower Address
		LITTLE_ENDIAN; //LSB is stored at lower Address
	}

	public static boolean bitTest(byte b,int pos) {
		if (pos>7 | pos<0) throw new InvalidParameterException("Invalid bit position");
		return (b & (1 << (7 - pos))) != 0;
	}

	public static byte[] binToBytes(String prettyBin) {
		return binToBytes(prettyBin, ENDIANESS.BIG_ENDIAN);
	}

	public static byte[] binToBytes(String prettyBin,ENDIANESS endianess) {
		int cpt = 0;
		char[] chars = prettyBin.toCharArray();

		if (chars.length==0) return new byte[0];

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

		int l = ((len-1) / 8) + 1;

		byte[] data = new byte[l];

		for (int i = 0; i < len; i++) {
			int index;
			if (endianess == ENDIANESS.LITTLE_ENDIAN) {
				index = i/8;
			}
			else {
				index = l-1-(i/8);
			}

			//Read in reverse order (bit order are always BIG_ENDIAN form (first bit in sequence is MSB)
			if (charsnospace[len-1-i] == '1')
				data[index] += (byte) (1<<(i%8));
			else if (charsnospace[len-1-i] == '0')
				data[index] += 0;
			else
				throw new InvalidParameterException("Invalid binary char founded in "+prettyBin+" (not 0 or 1) at pos "+(len-i));

		}

		return data;
	}

	public static String bytesToBin(byte[] byteArray,ENDIANESS endianess) {
//		int spaces = byteArray.length;
		char[] chars = new char[9*byteArray.length];
		for (int j = 0; j < byteArray.length; j++) {
			byte b ;
			if (endianess == ENDIANESS.BIG_ENDIAN)
				b=byteArray[j];
			else
				b=byteArray[byteArray.length-1-j];

			for (int i = 0; i < 8; i++) {
				chars[j*9+i] = (b & (1 << (7 - i))) == 0 ? '0' : '1';
			}
			chars[j*9+8] = ' ';
		}
		return String.valueOf(chars).trim();
	}

	public static String byteToBin(byte b) {
		char[] chars = new char[8];
		for (int i = 0; i <8; i++) {
			chars[i] = (b & (1 << (7-i))) == 0 ? '0' : '1';
		}
		return String.valueOf(chars);
	}

}
