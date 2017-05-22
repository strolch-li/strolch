import java.io.UnsupportedEncodingException;
import java.security.SecureRandom;
import java.text.MessageFormat;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

public class Temp {

	public static void main(String[] args) throws Exception {

		for (int i = 0; i < 10; i++) {

			long start = System.nanoTime();

			byte[] bytes = new byte[16];
			SecureRandom secureRandom = new SecureRandom();
			secureRandom.nextBytes(bytes);
			String token = getHexString(bytes);
			System.out.println(token);

			String algorithm = "PBKDF2WithHmacSHA512";
			char[] password = "admin".toCharArray();
			byte[] salt = "admin".getBytes();
			int iterations = 10000;
			int keyLength = 256;

			SecretKeyFactory skf = SecretKeyFactory.getInstance(algorithm);
			PBEKeySpec spec = new PBEKeySpec(password, salt, iterations, keyLength);
			SecretKey key = skf.generateSecret(spec);
			byte[] res = key.getEncoded();

			System.out.println("Password hash: " + getHexString(res));
			System.out.println("Salt: " + getHexString(salt));

			long end = System.nanoTime();
			System.out.println("Took: " + formatNanoDuration(end - start));
		}
	}

	public static String getHexString(byte[] raw) throws RuntimeException {
		try {
			byte[] hex = new byte[2 * raw.length];
			int index = 0;

			for (byte b : raw) {
				int v = b & 0xFF;
				hex[index++] = HEX_CHAR_TABLE[v >>> 4];
				hex[index++] = HEX_CHAR_TABLE[v & 0xF];
			}

			return new String(hex, "ASCII"); //$NON-NLS-1$

		} catch (UnsupportedEncodingException e) {
			String msg = MessageFormat.format("Something went wrong while converting to HEX: {0}", e.getMessage()); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		}
	}

	public static String formatNanoDuration(final long nanos) {
		if (nanos >= 3600000000000L) {
			return String.format("%.0fh", (nanos / 3600000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 60000000000L) {
			return String.format("%.0fm", (nanos / 60000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000000000L) {
			return String.format("%.0fs", (nanos / 1000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000000L) {
			return String.format("%.0fms", (nanos / 1000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000L) {
			return String.format("%.0fus", (nanos / 1000.0D)); //$NON-NLS-1$
		} else {
			return nanos + "ns"; //$NON-NLS-1$
		}
	}

	private static final byte[] HEX_CHAR_TABLE = { (byte) '0', (byte) '1', (byte) '2', (byte) '3', (byte) '4',
			(byte) '5', (byte) '6', (byte) '7', (byte) '8', (byte) '9', (byte) 'a', (byte) 'b', (byte) 'c', (byte) 'd',
			(byte) 'e', (byte) 'f' };
}
