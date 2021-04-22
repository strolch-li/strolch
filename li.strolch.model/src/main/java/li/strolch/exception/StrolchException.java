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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchException extends RuntimeException {

	private I18nMessage i18n;

	public StrolchException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchException(String message) {
		super(message);
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
}
