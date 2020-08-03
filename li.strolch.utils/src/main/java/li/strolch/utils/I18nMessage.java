package li.strolch.utils;

import static li.strolch.utils.helper.StringHelper.EMPTY;

import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;

import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class I18nMessage {

	private static final Logger logger = LoggerFactory.getLogger(I18nMessage.class);

	private final String key;
	private final Properties values;
	private final ResourceBundle bundle;
	private String message;

	public I18nMessage(ResourceBundle bundle, String key) {
		DBC.INTERIM.assertNotNull("bundle must be set!", bundle);
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		this.key = key;
		this.values = new Properties();
		this.bundle = bundle;
	}

	public I18nMessage(String key, Properties values, String message) {
		DBC.INTERIM.assertNotEmpty("key must be set!", key);
		DBC.INTERIM.assertNotEmpty("message must be set!", message);
		this.key = key;
		this.values = values == null ? new Properties() : values;
		this.message = message;
		this.bundle = null;
	}

	public I18nMessage(I18nMessage i18nMessage) {
		this.key = i18nMessage.key;
		this.values = i18nMessage.values;
		this.bundle = i18nMessage.bundle;
		this.message = i18nMessage.message;
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

	private ResourceBundle getBundle(Locale locale) {
		if (this.bundle.getLocale() == locale)
			return this.bundle;
		String baseName = this.bundle.getBaseBundleName();

		try {

			ClassLoader classLoader = this.bundle.getClass().getClassLoader();
			if (classLoader == null)
				return ResourceBundle.getBundle(baseName, locale);
			return ResourceBundle.getBundle(baseName, locale, classLoader);

		} catch (MissingResourceException e) {
			logger.error("Failed to find resource bundle " + baseName + " " + locale.toLanguageTag()
					+ ", returning current bundle " + this.bundle.getLocale().toLanguageTag());
			return this.bundle;
		}
	}

	public String getMessage(ResourceBundle bundle) {
		return formatMessage(bundle);
	}

	public String getMessage(Locale locale) {
		return formatMessage(getBundle(locale));
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

		if (this.bundle == null) {
			this.message = this.key;
			return this.message;
		}

		this.message = formatMessage(this.bundle);
		return this.message;
	}

	public String formatMessage(ResourceBundle bundle) {
		try {
			String string = bundle.getString(this.key);
			return StringHelper.replacePropertiesIn(this.values, EMPTY, string);
		} catch (MissingResourceException e) {
			String baseName = bundle.getBaseBundleName();
			String languageTag = bundle.getLocale().toLanguageTag();
			logger.error("Key " + this.key + " is missing in bundle " + baseName + " for locale " + languageTag);
			return this.key;
		}
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
