package li.strolch.exception;

import java.util.Locale;

import li.strolch.utils.I18nMessage;

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
