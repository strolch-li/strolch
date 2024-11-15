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

package li.strolch.privilege.handler;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.internal.User;

import java.util.Map;

public interface SingleSignOnHandler {

	/**
	 * Initialize the concrete {@link SingleSignOnHandler}. The passed parameter map contains any configuration the
	 * concrete {@link SingleSignOnHandler} might need
	 *
	 * @param parameterMap a map containing configuration properties
	 */
	void initialize(PersistenceHandler persistenceHandler, Map<String, String> parameterMap);

	/**
	 * Authenticates a user on a remote Single Sign On service.
	 *
	 * @param data the data required to sign on the user
	 *
	 * @return the user, configured with the remote
	 *
	 * @throws PrivilegeException if the SSO can not be performed with the given data
	 */
	User authenticateSingleSignOn(Object data) throws PrivilegeException;

	/**
	 * Returns the configuration for this {@link SingleSignOnHandler}
	 *
	 * @return the configuration as a Map
	 */
	Map<String, String> getParameterMap();
}
