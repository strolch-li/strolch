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
package li.strolch.privilege.base;

/**
 * Exception thrown if access is denied during login, or if a certain privilege is not granted
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AccessDeniedException extends PrivilegeException {

	private static final long serialVersionUID = 1L;

	/**
	 * @param msg
	 *            detail on why and where access was denied
	 */
	public AccessDeniedException(String msg) {
		super(msg);
	}

	/**
	 * @param msg
	 *            detail on why and where access was denied
	 * @param e
	 *            root exception
	 */
	public AccessDeniedException(String msg, Exception e) {
		super(msg, e);
	}
}
