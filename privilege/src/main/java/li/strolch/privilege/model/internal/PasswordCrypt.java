package li.strolch.privilege.model.internal;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static li.strolch.utils.helper.StringHelper.*;

public class PasswordCrypt {

	private final byte[] password;
	private final byte[] salt;
	private final String hashAlgorithm;
	private final int hashIterations;
	private final int hashKeyLength;

	private static final Logger logger = LoggerFactory.getLogger(PasswordCrypt.class);

	public PasswordCrypt(byte[] password, byte[] salt) {
		this.password = password;
		this.salt = salt;
		this.hashAlgorithm = null;
		this.hashIterations = -1;
		this.hashKeyLength = -1;
	}

	public PasswordCrypt(byte[] password, byte[] salt, String hashAlgorithm, int hashIterations, int hashKeyLength) {
		this.password = password;
		this.salt = salt;
		this.hashAlgorithm = hashAlgorithm;
		this.hashIterations = hashIterations;
		this.hashKeyLength = hashKeyLength;
	}

	public byte[] getPassword() {
		return password;
	}

	public byte[] getSalt() {
		return salt;
	}

	public String getHashAlgorithm() {
		return hashAlgorithm;
	}

	public int getHashIterations() {
		return hashIterations;
	}

	public int getHashKeyLength() {
		return hashKeyLength;
	}

	public static PasswordCrypt parse(String passwordS, String saltS) {
		if (isEmpty(passwordS))
			return null;

		byte[] salt = null;
		if (isNotEmpty(saltS))
			salt = fromHexString(saltS.trim());

		if (isEmpty(passwordS))
			return new PasswordCrypt(null, salt);

		passwordS = passwordS.trim();

		byte[] password;
		if (!passwordS.startsWith("$")) {
			password = fromHexString(passwordS);
			return new PasswordCrypt(password, salt);
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

	@Override
	public String toString() {
		return buildPasswordString();
	}

	public String buildPasswordString() {
		if (this.password == null || this.salt == null || this.hashAlgorithm == null || this.hashIterations == -1 ||
				this.hashKeyLength == -1) {
			return null;
		}

		return buildPasswordString(getHashAlgorithm(), getHashIterations(), getHashKeyLength(), getSalt(),
				getPassword());

	}

	public static String buildPasswordString(String hashAlgorithm, int hashIterations, int hashKeyLength, byte[] salt,
			byte[] passwordArr) {
		String algo = hashAlgorithm + "," + hashIterations + "," + hashKeyLength;
		String hash = toHexString(salt);
		String password = toHexString(passwordArr);
		return "$" + algo + "$" + hash + "$" + password;
	}
}
