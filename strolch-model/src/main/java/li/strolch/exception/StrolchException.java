/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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

import java.util.Locale;
import java.util.ResourceBundle;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchException extends RuntimeException {

	protected I18nMessage i18n;

	public StrolchException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchException(String message) {
		super(message);
	}

	public StrolchException(I18nMessage i18n) {
		super(i18n.getMessage(Locale.getDefault()));
		this.i18n = i18n;
		if (i18n.hasException())
			initCause(i18n.getException());
	}

	public StrolchException(I18nMessage i18n, Throwable cause) {
		super(i18n.getMessage(Locale.getDefault()), cause);
		this.i18n = i18n;
	}

	public StrolchException(ResourceBundle bundle, String key) {
		this(new I18nMessage(bundle, key));
	}

	public StrolchException(ResourceBundle bundle, String key, String prop, Object value) {
		this(new I18nMessage(bundle, key).value(prop, value));
	}

	public StrolchException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2,
			Object value2) {
		this(new I18nMessage(bundle, key).value(prop1, value1).value(prop2, value2));
	}

	public StrolchException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2, Object value2,
			String prop3, Object value3) {
		this(new I18nMessage(bundle, key).value(prop1, value1).value(prop2, value2).value(prop3, value3));
	}

	public StrolchException(ResourceBundle bundle, String key, String prop1, Object value1, String prop2, Object value2,
			String prop3, Object value3, String prop4, Object value4) {
		this(new I18nMessage(bundle, key)
				.value(prop1, value1)
				.value(prop2, value2)
				.value(prop3, value3)
				.value(prop4, value4));
	}

	public boolean hasI18n() {
		return this.i18n != null;
	}

	public I18nMessage getI18n() {
		return this.i18n;
	}

	public void setI18n(I18nMessage i18n) {
		this.i18n = i18n;
	}

	public StrolchException i18n(I18nMessage i18n) {
		this.i18n = i18n;
		return this;
	}

	public StrolchException cause(Throwable cause) {
		initCause(cause);
		return this;
	}
}
