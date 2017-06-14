package li.strolch.utils;

import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.Properties;
import java.util.ResourceBundle;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

public class I18nMessage {

	private String bundleId;
	private String key;
	private Properties values;

	public I18nMessage(String bundleId, String key) {
		DBC.INTERIM.assertNotEmpty("bundleId must be set!", bundleId);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.bundleId = bundleId;
		this.key = key;
		this.values = new Properties();
	}

	public String getBundleId() {
		return this.bundleId;
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
		ResourceBundle bundle = ResourceBundle.getBundle(this.bundleId);
		String string = bundle.getString(this.key);
		return StringHelper.replacePropertiesIn(this.values, EMPTY, string);
	}
}
