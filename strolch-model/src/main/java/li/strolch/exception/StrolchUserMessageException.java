package li.strolch.exception;

import li.strolch.utils.I18nMessage;

import java.util.Locale;
import java.util.ResourceBundle;

public class StrolchUserMessageException extends StrolchException {

	public StrolchUserMessageException(I18nMessage i18n) {
		super(i18n);
	}

	public StrolchUserMessageException(I18nMessage i18n, Throwable cause) {
		super(i18n, cause);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key) {
		super(bundle, key);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop, Object value) {
		super(bundle, key, prop, value);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2) {
		super(bundle, key, prop1, value1, prop2, value2);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3) {
		super(bundle, key, prop1, value1, prop2, value2, prop3, value3);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3, String prop4, Object value4) {
		super(bundle, key, prop1, value1, prop2, value2, prop3, value3, prop4, value4);
	}
}
