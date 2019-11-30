package li.strolch.privilege.helper;

import li.strolch.privilege.model.internal.User;
import li.strolch.utils.helper.StringHelper;

public class CryptHelper {

	public static String buildPasswordString(User user) {
		return buildPasswordString(user.getHashAlgorithm(), user.getHashIterations(), user.getHashKeyLength(),
				user.getSalt(), user.getPassword());
	}

	public static String buildPasswordString(String hashAlgorithm, int hashIterations, int hashKeyLength, byte[] salt,
			byte[] passwordArr) {
		String algo = hashAlgorithm + "," + hashIterations + "," + hashKeyLength;
		String hash = StringHelper.toHexString(salt);
		String password = StringHelper.toHexString(passwordArr);
		return "$" + algo + "$" + hash + "$" + password;
	}
}
