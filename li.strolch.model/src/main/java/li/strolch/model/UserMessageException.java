package li.strolch.model;

import java.util.Locale;

import li.strolch.utils.I18nMessage;
import li.strolch.utils.dbc.DBC;

public class UserMessageException extends Exception {

	private final I18nMessage i18nMsg;

	public UserMessageException(I18nMessage i18nMsg) {
		super(i18nMsg.getMessage(Locale.getDefault()));
		DBC.PRE.assertNotNull("i18nMsg must not be null!", i18nMsg);
		this.i18nMsg = i18nMsg;
	}

	public I18nMessage getI18nMsg() {
		return this.i18nMsg;
	}
}
