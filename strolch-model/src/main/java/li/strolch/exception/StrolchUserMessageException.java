/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.exception;

import li.strolch.utils.I18nMessage;

import java.util.ResourceBundle;

public class StrolchUserMessageException extends StrolchException {

	public StrolchUserMessageException(I18nMessage i18n) {
		super(i18n);
	}

	public StrolchUserMessageException(I18nMessage i18n, Throwable cause) {
		super(i18n, cause);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key) {
		super(bundle, key);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop, Object value) {
		super(bundle, key, prop, value);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2) {
		super(bundle, key, prop1, value1, prop2, value2);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3) {
		super(bundle, key, prop1, value1, prop2, value2, prop3, value3);
	}

	public StrolchUserMessageException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2, String prop3, Object value3, String prop4, Object value4) {
		super(bundle, key, prop1, value1, prop2, value2, prop3, value3, prop4, value4);
	}
}
