package li.strolch.privilege.helper;

import static li.strolch.privilege.base.PrivilegeConstants.DEFAULT_SMALL_ITERATIONS;
import static li.strolch.utils.helper.StringHelper.fromHexString;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

public class Crypt {

	private String algorithm;
	private int keyLength;
	private int iterations;
	private byte[] salt;
	private byte[] password;

	public Crypt() {
		// nothing to do
	}

	public String getAlgorithm() {
		return algorithm;
	}

	public Crypt setAlgorithm(String algorithm) {
		this.algorithm = algorithm;
		return this;
	}

	public byte[] getSalt() {
		return salt;
	}

	public Crypt setSalt(byte[] salt) {
		this.salt = salt;
		return this;
	}

	public int getKeyLength() {
		return this.keyLength;
	}

	public Crypt setKeyLength(int keyLength) {
		this.keyLength = keyLength;
		return this;
	}

	public int getIterations() {
		return this.iterations;
	}

	public Crypt setIterations(int iterations) {
		this.iterations = iterations;
		return this;
	}

	public byte[] getPassword() {
		return password;
	}

	public Crypt setPassword(byte[] password) {
		this.password = password;
		return this;
	}

	public Crypt parseCrypt(String crypt) {
		DBC.PRE.assertNotEmpty("crypt can no be empty", crypt);

		if (crypt.contains("$")) {
			String[] parts = crypt.split("\\$");

			if (parts.length == 5) {

				setAlgorithm(parts[1], true);

				Map<String, String> algOptions = parseAlgOptions(parts[2]);
				if (algOptions == null)
					this.iterations = DEFAULT_SMALL_ITERATIONS;
				else
					this.iterations = Integer.parseInt(algOptions.get("rounds"));

				this.salt = fromHexString(parts[3]);
				this.password = fromHexString(parts[4]);

			} else if (parts.length == 4) {

				setAlgorithm(parts[1], true);
				this.iterations = DEFAULT_SMALL_ITERATIONS;
				this.salt = fromHexString(parts[2]);
				this.password = fromHexString(parts[3]);

			} else if (parts.length == 3) {

				setAlgorithm(parts[1], false);
				this.password = fromHexString(parts[2]);

			} else {
				throw new IllegalStateException("Wrong number of $ chars in " + crypt + ": " + parts.length);
			}

		} else {

			this.algorithm = "SHA-512";
			this.password = fromHexString(crypt);
		}

		return this;
	}

	public void assertSame(char[] password) {
		if (!isSame(password))
			throw new IllegalStateException("Passwords not the same");
	}

	public boolean isSame(char[] password) {
		if (this.password == null)
			throw new IllegalStateException("password not set, call parseCrypt() first!");
		if (password == null)
			throw new IllegalStateException("password must not be null");

		try {

			byte[] hash;
			if (this.salt == null) {

				hash = StringHelper.hash(this.algorithm, new String(password).getBytes());

			} else {

				PBEKeySpec spec = new PBEKeySpec(password, this.salt, this.iterations, this.keyLength);
				SecretKeyFactory skf = SecretKeyFactory.getInstance(this.algorithm);
				SecretKey key = skf.generateSecret(spec);
				hash = key.getEncoded();
			}

			return Arrays.equals(hash, this.password);

		} catch (Exception e) {
			throw new IllegalStateException("Failed validation password for algorithm " + this.algorithm, e);
		}
	}

	public String toCryptString() {

		StringBuilder sb = new StringBuilder();

		sb.append("$");
		switch (this.algorithm) {
		case "MD5":
			sb.append("1");
			break;

		case "PBKDF2WithHmacSHA256":
		case "SHA-256":
			sb.append("5");
			break;

		case "PBKDF2WithHmacSHA512":
		case "SHA-512":
			sb.append("6");
			break;

		default:
			throw new IllegalStateException("Unhandled algorithm " + this.algorithm);
		}

		if (this.iterations != 0 && this.iterations != DEFAULT_SMALL_ITERATIONS) {
			sb.append("$");
			sb.append("rounds");
			sb.append("=");
			sb.append(iterations);
		}

		if (this.salt != null) {
			sb.append("$");
			sb.append(StringHelper.toHexString(this.salt));
		}

		sb.append("$");
		sb.append(StringHelper.toHexString(this.password));

		return sb.toString();
	}

	private Map<String, String> parseAlgOptions(String part) {
		String[] options = part.split(",");
		Map<String, String> algOptions = new HashMap<>(options.length);
		for (String option : options) {
			if (option.trim().isEmpty())
				continue;
			if (!option.contains("="))
				throw new IllegalStateException("Option " + option + " is missing = char");
			String[] keyValue = option.split("=");
			algOptions.put(keyValue[0].trim(), keyValue[1].trim());
		}

		return algOptions;
	}

	private void setAlgorithm(String id, boolean hasSalt) {
		switch (id) {
		case "1":
			this.algorithm = "MD5";
			this.keyLength = 0;
			break;
		case "5":
			this.algorithm = hasSalt ? "PBKDF2WithHmacSHA256" : "SHA-256";
			this.keyLength = 256;
			break;
		case "6":
			this.algorithm = hasSalt ? "PBKDF2WithHmacSHA512" : "SHA-512";
			this.keyLength = 256;
			break;

		default:
			throw new IllegalStateException("Unhandled ID " + id);
		}
	}
}
