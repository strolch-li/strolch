package li.strolch.utils;

import java.util.Locale;
import java.util.ResourceBundle;

public class I18nUtils {
	private static final String BUNDLE = "strolch-utils";

	public static final ResourceBundle i18nUtils = ResourceBundle.getBundle(BUNDLE);

	public static ResourceBundle getBundle(Locale locale) {
		return ResourceBundle.getBundle(BUNDLE, locale);
	}

	public static String i18n(Locale locale, String key) {
		ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE, locale);
		if (bundle.containsKey(key))
			return bundle.getString(key);
		return key;
	}
}
