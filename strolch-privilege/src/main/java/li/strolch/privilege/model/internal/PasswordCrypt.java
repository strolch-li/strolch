package li.strolch.privilege.model.internal;

import static li.strolch.utils.helper.StringHelper.*;

public record PasswordCrypt(byte[] password, byte[] salt, String hashAlgorithm, int hashIterations, int hashKeyLength) {

	@Override
	public String toString() {
		return buildPasswordString();
	}

	public String buildPasswordString() {
		if (this.password == null || this.salt == null || this.hashAlgorithm == null || this.hashIterations == -1 ||
				this.hashKeyLength == -1) {
			return null;
		}

		return buildPasswordString(this.hashAlgorithm, this.hashIterations, this.hashKeyLength, this.salt,
				this.password);
	}

	public static String buildPasswordString(String hashAlgorithm, int hashIterations, int hashKeyLength, byte[] salt,
			byte[] passwordArr) {
		String algo = hashAlgorithm + "," + hashIterations + "," + hashKeyLength;
		String hash = toHexString(salt);
		String password = toHexString(passwordArr);
		return "$" + algo + "$" + hash + "$" + password;
	}

	public static PasswordCrypt of(byte[] password, byte[] salt) {
		return new PasswordCrypt(password, salt, null, -1, -1);
	}

	public static PasswordCrypt parse(String passwordS, String saltS) {
		if (isEmpty(passwordS))
			return null;

		byte[] salt = null;
		if (isNotEmpty(saltS))
			salt = fromHexString(saltS.trim());

		if (isEmpty(passwordS))
			return PasswordCrypt.of(null, salt);

		passwordS = passwordS.trim();

		byte[] password;
		if (!passwordS.startsWith("$")) {
			password = fromHexString(passwordS);
			return PasswordCrypt.of(password, salt);
		}

		String[] parts = passwordS.split("\\$");
		if (parts.length != 4)
			throw new IllegalArgumentException(
					"Illegal password " + passwordS + ": Starts with $, but does not have 3 parts!");

		String hashAlgorithmS = parts[1];
		String[] hashParts = hashAlgorithmS.split(",");

		if (hashParts.length != 3)
			throw new IllegalArgumentException(
					"Illegal password " + passwordS + ": hashAlgorithm part does not have 3 parts separated by comma!");

		String hashAlgorithm = hashParts[0];
		int hashIterations = Integer.parseInt(hashParts[1]);
		int hashKeyLength = Integer.parseInt(hashParts[2]);

		salt = fromHexString(parts[2]);
		password = fromHexString(parts[3]);

		return new PasswordCrypt(password, salt, hashAlgorithm, hashIterations, hashKeyLength);
	}
}
