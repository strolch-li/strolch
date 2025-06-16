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

package li.strolch.privilege.base;

/**
 * Exception thrown if the given credentials are invalid
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InvalidCredentialsException extends AccessDeniedException {

	/**
	 * @param msg the message to accompany the exception
	 */
	public InvalidCredentialsException(String msg) {
		super(msg);
	}

	/**
	 * @param msg detail on why and where access was denied
	 * @param e   root exception
	 */
	public InvalidCredentialsException(String msg, Exception e) {
		super(msg, e);
	}
}
