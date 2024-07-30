package li.strolch.privilege.handler;

import static java.lang.Boolean.parseBoolean;
import static java.lang.Integer.parseInt;
import static li.strolch.privilege.i18n.PrivilegeMessages.getString;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.Map;

public class BasicPasswordStrengthHandler implements PasswordStrengthHandler {

	protected int minLength;
	protected int maxLength;
	protected boolean needsNumbers;
	protected boolean needsLowerCase;
	protected boolean needsUpperCase;
	protected boolean needsSpecialChars;

	@Override
	public void initialize(Map<String, String> parameterMap) {
		this.minLength = parseInt(parameterMap.getOrDefault("minLength", "8"));
		this.maxLength = parseInt(parameterMap.getOrDefault("maxLength", String.valueOf(1024)));
		this.needsNumbers = parseBoolean(parameterMap.getOrDefault("needsNumbers", "true"));
		this.needsLowerCase = parseBoolean(parameterMap.getOrDefault("needsLowerCase", "true"));
		this.needsUpperCase = parseBoolean(parameterMap.getOrDefault("needsUpperCase", "true"));
		this.needsSpecialChars = parseBoolean(parameterMap.getOrDefault("needsSpecialChars", "false"));

		if (this.minLength < 8)
			throw new IllegalStateException("minLength can not be less than 8");
		if (this.maxLength > 1024)
			throw new IllegalStateException("maxLength can not be greater than 1024");
	}

	@Override
	public String getDescription(Locale locale) {
		String description;

		if (this.maxLength < 100)
			description = MessageFormat.format(getString(locale, "Privilege.passwordLengthBetween"), this.minLength,
					this.maxLength);
		else
			description = MessageFormat.format(getString(locale, "Privilege.passwordLengthAtLeast"), this.minLength);

		if (this.needsNumbers)
			description += ", " + getString(locale, "Privilege.passwordMustContainNumbers");

		if (this.needsLowerCase && this.needsUpperCase)
			description += ", " + getString(locale, "Privilege.passwordMustContainLowerAndUpperCase");
		else if (this.needsLowerCase)
			description += ", " + getString(locale, "Privilege.passwordMustContainLowerCase");
		else
			description += ", " + getString(locale, "Privilege.passwordMustContainUpperCase");

		if (this.needsSpecialChars)
			description += ", " + getString(locale, "Privilege.passwordMustContainSpecialCharacters");

		return description;
	}

	@Override
	public boolean validateStrength(char[] password) {
		if (password.length < this.minLength || password.length > this.maxLength)
			return false;

		boolean numbersOk = !this.needsNumbers;
		boolean lowerCaseOk = !this.needsLowerCase;
		boolean upperCaseOk = !this.needsUpperCase;
		boolean specialCharsOk = !this.needsSpecialChars;

		for (char c : password) {

			if (numbersOk && lowerCaseOk && upperCaseOk && specialCharsOk)
				return true;

			if (!numbersOk && Character.isDigit(c)) {
				numbersOk = true;
				continue;
			}

			if (!lowerCaseOk && Character.isLowerCase(c)) {
				lowerCaseOk = true;
				continue;
			}

			if (!upperCaseOk && Character.isUpperCase(c)) {
				upperCaseOk = true;
				continue;
			}

			if (!specialCharsOk && isSpecial(c))
				specialCharsOk = true;
		}

		return numbersOk && lowerCaseOk && upperCaseOk && specialCharsOk;
	}

	public static boolean isSpecial(char c) {
		return !(Character.isDigit(c) || Character.isLowerCase(c) || Character.isUpperCase(c));
	}
}
