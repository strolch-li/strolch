package li.strolch.utils;

import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.Properties;
import java.util.ResourceBundle;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

public class I18nMessage {

	private ResourceBundle bundle;
	private String key;
	private Properties values;

	public I18nMessage(ResourceBundle bundle, String key) {
		DBC.INTERIM.assertNotNull("bundle must be set!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.bundle = bundle;
		this.key = key;
		this.values = new Properties();
	}

	public String getKey() {
		return this.key;
	}

	public Properties getValues() {
		return this.values;
	}

	public I18nMessage value(String key, String value) {
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("value must be set!", value);
		this.values.setProperty(key, value);
		return this;
	}

	public String formatMessage() {
		String string = this.bundle.getString(this.key);
		return StringHelper.replacePropertiesIn(this.values, EMPTY, string);
	}
}
