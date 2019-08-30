package li.strolch.utils;

import java.util.Locale;
import java.util.ResourceBundle;

public class I18nStrolchUtilsBundle {

	public static String i18n(Locale locale, String key) {
		ResourceBundle bundle = ResourceBundle.getBundle("strolch-utils", locale);
		if (bundle.containsKey(key))
			return bundle.getString(key);
		return key;
	}
}
