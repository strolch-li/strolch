package li.strolch.utils;

import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class I18nMessage {

	private static final Logger logger = LoggerFactory.getLogger(I18nMessage.class);

	private ResourceBundle bundle;
	private String key;
	private Properties values;
	private String message;

	public I18nMessage(ResourceBundle bundle, String key) {
		DBC.INTERIM.assertNotNull("bundle must be set!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.bundle = bundle;
		this.key = key;
		this.values = new Properties();
	}

	public I18nMessage(String key, Properties values, String message) {
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("message must be set!", message);
		this.key = key;
		this.values = values == null ? new Properties() : values;
		this.message = message;
	}

	public String getKey() {
		return this.key;
	}

	public Properties getValues() {
		return this.values;
	}

	public Object getValue(String key) {
		return this.values.get(key);
	}

	public String getMessage() {
		return formatMessage();
	}

	public I18nMessage value(String key, Object value) {
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.values.setProperty(key, value == null ? "(null)" : value.toString());
		return this;
	}

	public String formatMessage() {
		if (this.message != null)
			return this.message;

		try {
			String string = this.bundle.getString(this.key);
			this.message = StringHelper.replacePropertiesIn(this.values, EMPTY, string);
		} catch (MissingResourceException e) {
			logger.error("Key " + this.key + " is missing in bundle " + this.bundle.getBaseBundleName());
			this.message = this.key;
		}

		return this.message;
	}

	public <T> T accept(I18nMessageVisitor<T> visitor) {
		return visitor.visit(this);
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
