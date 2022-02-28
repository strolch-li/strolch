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

import java.util.Locale;

import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Restrictable;
import li.strolch.utils.I18nMessage;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchAccessDeniedException extends StrolchException {

	private final Certificate certificate;
	private final Restrictable restrictable;
	private final I18nMessage i18n;

	public StrolchAccessDeniedException(Certificate certificate, Restrictable restrictable, I18nMessage i18n,
			Throwable cause) {
		super(i18n.getMessage(), cause);
		this.certificate = certificate;
		this.restrictable = restrictable;
		this.i18n = i18n;
	}

	public Certificate getCertificate() {
		return certificate;
	}

	public Restrictable getRestrictable() {
		return restrictable;
	}

	public I18nMessage getI18n() {
		return this.i18n;
	}
}
