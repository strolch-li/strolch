/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.service;

import java.util.Locale;
import java.util.ResourceBundle;

public class I18nService {
	private static final String BUNDLE = "strolch-service";

	public static final ResourceBundle i18nService = ResourceBundle.getBundle(BUNDLE);

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
