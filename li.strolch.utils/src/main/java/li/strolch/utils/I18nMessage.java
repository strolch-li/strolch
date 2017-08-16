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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((this.key == null) ? 0 : this.key.hashCode());
		result = prime * result + ((this.values == null) ? 0 : this.values.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		I18nMessage other = (I18nMessage) obj;
		if (this.key == null) {
			if (other.key != null)
				return false;
		} else if (!this.key.equals(other.key))
			return false;
		if (this.values == null) {
			if (other.values != null)
				return false;
		} else if (!this.values.equals(other.values))
			return false;
		return true;
	}
}
