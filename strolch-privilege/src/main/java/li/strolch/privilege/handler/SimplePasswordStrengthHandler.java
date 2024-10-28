package li.strolch.privilege.handler;

import java.util.Locale;
import java.util.Map;

public class SimplePasswordStrengthHandler implements PasswordStrengthHandler {

	@Override
	public void initialize(Map<String, String> parameterMap) {
		// do nothing
	}

	@Override
	public String getDescription(Locale locale) {
		return "Password must be at least 3 characters long";
	}

	@Override
	public boolean validateStrength(char[] password) {
		return password != null && password.length >= 3;
	}
}
