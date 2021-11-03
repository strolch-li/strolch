package li.strolch.exception;

import java.util.Locale;
import java.util.ResourceBundle;

import li.strolch.utils.I18nMessage;

public class StrolchUserMessageException extends StrolchException {

	private I18nMessage i18n;

	public StrolchUserMessageException(I18nMessage i18n) {
		super(i18n.getMessage(Locale.getDefault()));
		this.i18n = i18n;
		if (i18n.hasException())
			initCause(i18n.getException());
	}

	public StrolchUserMessageException(I18nMessage i18n, Throwable cause) {
		super(i18n.getMessage(Locale.getDefault()), cause);
		this.i18n = i18n;
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key) {
		this(new I18nMessage(bundle, key));
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop, Object value) {
		this(new I18nMessage(bundle, key) //
				.value(prop, value));
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2) {
		this(new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2));
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3) {
		this(new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2) //
				.value(prop3, value3));
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3, String prop4, Object value4) {
		this(new I18nMessage(bundle, key) //
				.value(prop1, value1) //
				.value(prop2, value2) //
				.value(prop3, value3) //
				.value(prop4, value4));
	}

	public StrolchUserMessageException cause(Throwable cause) {
		initCause(cause);
		return this;
	}

	public boolean hasI18n() {
		return this.i18n != null;
	}

	public I18nMessage getI18n() {
		return this.i18n;
	}

	public void setI18n(I18nMessage i18n) {
		this.i18n = i18n;
	}

	public StrolchUserMessageException i18n(I18nMessage i18n) {
		this.i18n = i18n;
		return this;
	}
}
