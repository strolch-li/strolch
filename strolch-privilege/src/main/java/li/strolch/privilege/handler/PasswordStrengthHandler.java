package li.strolch.privilege.handler;

import java.util.Locale;
import java.util.Map;

/**
 * The password strength handler allows to plug-in different algorithms for validating the strength of a password
 */
public interface PasswordStrengthHandler {

	/**
	 * Initialize the concrete {@link PasswordStrengthHandler}. The passed parameter map contains any configuration the
	 * concrete {@link PasswordStrengthHandler} might need
	 *
	 * @param parameterMap a map containing configuration properties
	 */
	void initialize(Map<String, String> parameterMap);

	/**
	 * Returns a description what a password must contain in order to be regarded as strong for this concrete
	 * implementation
	 *
	 * @param locale the locale in which to return the description
	 *
	 * @return a description of a strong password
	 */
	String getDescription(Locale locale);

	/**
	 * Performs the validation of the given password
	 *
	 * @param password the password to validate
	 *
	 * @return true if the password meets the criteria for a strong password
	 */
	boolean validateStrength(char[] password);
}
