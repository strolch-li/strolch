package li.strolch.utils;

import java.security.SecureRandom;

/**
 * Generates code which should be easily readable as problematic letters e.g. 0, o, O, i, I, l are left out
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class CodeGenerator {

	private static final String ALPHA_NUMERIC_UPPER = "123456789ABCDEFGHJKLMNPQRSTUVWXYZ";
	private static final String ALPHA_NUMERIC_LOWER_UPPER = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz";

	public static String alphaNumericUpper(int len) {

		SecureRandom rnd = new SecureRandom();

		StringBuilder sb = new StringBuilder(len);
		for (int i = 0; i < len; i++)
			sb.append(ALPHA_NUMERIC_UPPER.charAt(rnd.nextInt(ALPHA_NUMERIC_UPPER.length())));

		return sb.toString();
	}

	public static String alphaNumericLowerUpper(int len) {

		SecureRandom rnd = new SecureRandom();

		StringBuilder sb = new StringBuilder(len);
		for (int i = 0; i < len; i++)
			sb.append(ALPHA_NUMERIC_LOWER_UPPER.charAt(rnd.nextInt(ALPHA_NUMERIC_LOWER_UPPER.length())));

		return sb.toString();
	}
}
