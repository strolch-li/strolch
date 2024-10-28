package li.strolch.exception;

import li.strolch.utils.I18nMessage;

import java.util.Locale;

public class StrolchElementNotFoundException extends StrolchModelException {

	public StrolchElementNotFoundException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchElementNotFoundException(String message) {
		super(message);
	}

	public StrolchElementNotFoundException(I18nMessage i18n) {
		super(i18n.getMessage(Locale.getDefault()));
	}
}
