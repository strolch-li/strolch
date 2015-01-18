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

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.Restrictable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchAccessDeniedException extends StrolchException {

	private static final long serialVersionUID = 1L;

	private Certificate certificate;
	private Restrictable restrictable;

	/**
	 * 
	 * @param certificate
	 * @param restrictable
	 * @param message
	 * @param cause
	 */
	public StrolchAccessDeniedException(Certificate certificate, Restrictable restrictable, String message,
			Throwable cause) {
		super(message, cause);
		this.certificate = certificate;
		this.restrictable = restrictable;
	}

	/**
	 * 
	 * @param certificate
	 * @param restrictable
	 * @param message
	 */
	public StrolchAccessDeniedException(Certificate certificate, Restrictable restrictable, String message) {
		super(message);
		this.certificate = certificate;
		this.restrictable = restrictable;
	}

	public Certificate getCertificate() {
		return certificate;
	}

	public Restrictable getRestrictable() {
		return restrictable;
	}
}
