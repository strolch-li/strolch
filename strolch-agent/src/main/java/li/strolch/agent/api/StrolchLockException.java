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
package li.strolch.agent.api;

import li.strolch.exception.StrolchException;
import li.strolch.utils.I18nMessage;

import java.util.Locale;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchLockException extends StrolchException {

	public StrolchLockException(String message, Throwable cause) {
		super(message, cause);
	}

	public StrolchLockException(String message) {
		super(message);
	}

	public StrolchLockException(I18nMessage i18n) {
		super(i18n.getMessage(Locale.getDefault()));
	}
}
