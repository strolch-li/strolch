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
package li.strolch.runtime;

import static li.strolch.utils.helper.StringHelper.DOT;
import static li.strolch.utils.helper.StringHelper.UNDERLINE;

import li.strolch.agent.api.ObserverHandler;
import li.strolch.model.StrolchModelConstants;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.privilege.base.PrivilegeConstants;
import li.strolch.privilege.handler.PrivilegeHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class StrolchConstants extends StrolchModelConstants {

	public static final String STROLCH_ENV = "strolch.env";
	public static final String STROLCH_PATH = "strolch.path";

	public static final String ENV_STROLCH = "ENV_STROLCH";
	public static final String ENV_STROLCH_PATH = "ENV_STROLCH_PATH";
	public static final String PERSISTENCE_HANDLER = PersistenceHandler.class.getSimpleName();
	public static final String OBSERVER_HANDLER = ObserverHandler.class.getSimpleName();

	public static final String PRIVILEGE_HANDLER = "PrivilegeHandler";
	public static final String SYSTEM_USER_AGENT = "agent";

	public static final String PROP_REALM = PrivilegeConstants.REALM;
	public static final String DEFAULT_REALM = "defaultRealm";

	public static final String TYPE_STROLCH_JOB = "StrolchJob";

	public static String makeRealmKey(String realmName, String key) {
		return makeRealmKey(realmName, key, false);
	}

	public static String makeRealmKey(String realmName, String key, boolean useEnv) {
		String realmKey = key;
		if (!realmName.equals(DEFAULT_REALM))
			realmKey += (useEnv ? UNDERLINE : DOT) + realmName;
		return useEnv ? realmKey.replace(DOT, UNDERLINE).toUpperCase() : realmKey;
	}

	/**
	 * Constants used for Privilege management, configuration, etc.
	 *
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	public static class StrolchPrivilegeConstants {

		public static final String PRIVILEGE = "Privilege";
		public static final String CERTIFICATE = "Certificate";
		public static final String LOGIN = "Login";
		public static final String LOGOUT = "Logout";
		public static final String SESSION_TIME_OUT = "SessionTimeout";
		public static final String ROLE = "Role";
		public static final String USER = "User";

		public static final String PRIVILEGE_GET_ROLE = PrivilegeHandler.PRIVILEGE_GET_ROLE;
		public static final String PRIVILEGE_ADD_ROLE = PrivilegeHandler.PRIVILEGE_ADD_ROLE;
		public static final String PRIVILEGE_REMOVE_ROLE = PrivilegeHandler.PRIVILEGE_REMOVE_ROLE;
		public static final String PRIVILEGE_MODIFY_ROLE = PrivilegeHandler.PRIVILEGE_MODIFY_ROLE;

		public static final String PRIVILEGE_GET_USER = PrivilegeHandler.PRIVILEGE_GET_USER;
		public static final String PRIVILEGE_ADD_USER = PrivilegeHandler.PRIVILEGE_ADD_USER;
		public static final String PRIVILEGE_REMOVE_USER = PrivilegeHandler.PRIVILEGE_REMOVE_USER;
		public static final String PRIVILEGE_MODIFY_USER = PrivilegeHandler.PRIVILEGE_MODIFY_USER;
		public static final String PRIVILEGE_ADD_ROLE_TO_USER = PrivilegeHandler.PRIVILEGE_ADD_ROLE_TO_USER;
		public static final String PRIVILEGE_REMOVE_ROLE_FROM_USER = PrivilegeHandler.PRIVILEGE_REMOVE_ROLE_FROM_USER;
		public static final String PRIVILEGE_SET_USER_LOCALE = PrivilegeHandler.PRIVILEGE_SET_USER_LOCALE;
		public static final String PRIVILEGE_SET_USER_STATE = PrivilegeHandler.PRIVILEGE_SET_USER_STATE;
		public static final String PRIVILEGE_SET_USER_PASSWORD = PrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD;

		public static final String PRIVILEGE_INSPECTOR = "Inspector";

		public static final String PRIVILEGE_INVALIDATE_SESSION = "InvalidateSession";
		public static final String PRIVILEGE_GET_SESSION = "GetSession";

		public static final String PRIVILEGE_ADD_PREFIX = "Add";
		public static final String PRIVILEGE_UPDATE_PREFIX = "Update";
		public static final String PRIVILEGE_REMOVE_PREFIX = "Remove";
		public static final String PRIVILEGE_GET_PREFIX = "Get";
		public static final String PRIVILEGE_GET_NOTIFICATIONS = "GetNotifications";
		public static final String PRIVILEGE_GET_NOTIFICATION = "GetNotification";
		public static final String PRIVILEGE_GET_NOTIFICATIONS_ALL = "GetNotificationsAll";
	}
}
