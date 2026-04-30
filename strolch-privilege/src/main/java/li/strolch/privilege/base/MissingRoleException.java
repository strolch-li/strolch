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

import li.strolch.utils.I18nMessage;

import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Exception thrown when a required role is missing to perform a specific operation. This exception indicates that
 * access was denied due to the absence of the necessary role.
 */
public class MissingRoleException extends AccessDeniedException {
	private String user;
	private List<String> missingRoles;

	/**
	 * @param msg the message to accompany the exception
	 */
	public MissingRoleException(String msg, String user, List<String> missingRoles) {
		super(msg);
		this.user = user;
		this.missingRoles = missingRoles;
	}

	/**
	 * @param msg detail on why and where access was denied
	 * @param e   root exception
	 */
	public MissingRoleException(String msg, Exception e, String user, List<String> missingRoles) {
		super(msg, e);
		this.user = user;
		this.missingRoles = missingRoles;
	}

	public List<String> getMissingRoles() {
		return this.missingRoles;
	}

	public I18nMessage toI18n(Locale locale) {
		return new I18nMessage(ResourceBundle.getBundle("PrivilegeMessages", locale), "format.Privilege.noprivilege.role")
				.value("user", this.user)
				.value("role", String.join(", ", this.missingRoles));
	}
}
