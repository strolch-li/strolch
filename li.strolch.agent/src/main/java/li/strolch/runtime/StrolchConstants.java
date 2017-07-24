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

import li.strolch.agent.api.ObserverHandler;
import li.strolch.model.StrolchModelConstants;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.privilege.handler.PrivilegeHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class StrolchConstants {

	public static final String ENV_STROLCH = "ENV_STROLCH";
	public static final String PERSISTENCE_HANDLER = PersistenceHandler.class.getSimpleName();
	public static final String OBSERVER_HANDLER = ObserverHandler.class.getSimpleName();

	public static final String PRIVILEGE_HANDLER = "PrivilegeHandler";
	public static final String SYSTEM_USER_AGENT = "agent";

	public static final String PROP_REALM = "realm";
	public static final String DEFAULT_REALM = "defaultRealm";

	public static final String DEFAULT_XML_VERSION = StrolchModelConstants.DEFAULT_XML_VERSION;
	public static final String DEFAULT_ENCODING = StrolchModelConstants.DEFAULT_ENCODING;

	/**
	 * @see StrolchModelConstants#TEMPLATE
	 */
	public static final String TEMPLATE = StrolchModelConstants.TEMPLATE;

	/**
	 * @see StrolchModelConstants#INTERPRETATION_RESOURCE_REF
	 */
	public static final String INTERPRETATION_RESOURCE_REF = StrolchModelConstants.INTERPRETATION_RESOURCE_REF;

	/**
	 * @see StrolchModelConstants#INTERPRETATION_ORDER_REF
	 */
	public static final String INTERPRETATION_ORDER_REF = StrolchModelConstants.INTERPRETATION_ORDER_REF;

	/**
	 * @see StrolchModelConstants#INTERPRETATION_ACTIVITY_REF
	 */
	public static final String INTERPRETATION_ACTIVITY_REF = StrolchModelConstants.INTERPRETATION_ACTIVITY_REF;

	public static String makeRealmKey(String realmName, String key) {
		String realmKey = key;
		if (!realmName.equals(DEFAULT_REALM))
			realmKey += DOT + realmName;
		return realmKey;
	}

	/**
	 * Constants used for Privilege management, configuration, etc.
	 * 
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	public static class StrolchPrivilegeConstants {

		public static final String PRIVILEGE = "Privilege"; //$NON-NLS-1$
		public static final String CERTIFICATE = "Certificate"; //$NON-NLS-1$
		public static final String LOGIN = "Login"; //$NON-NLS-1$
		public static final String LOGOUT = "Logout"; //$NON-NLS-1$
		public static final String SESSION_TIME_OUT = "SessionTimeout"; //$NON-NLS-1$
		public static final String ROLE = "Role"; //$NON-NLS-1$
		public static final String USER = "User"; //$NON-NLS-1$

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

		public static final String PRIVILEGE_INVALIDATE_SESSION = "InvalidateSession";
		public static final String PRIVILEGE_GET_SESSION = "GetSession";

		public static final String PRIVILEGE_ADD_RESOURCE = "AddResource";
		public static final String PRIVILEGE_UPDATE_RESOURCE = "UpdateResource";
		public static final String PRIVILEGE_REMOVE_RESOURCE = "RemoveResource";

		public static final String PRIVILEGE_ADD_ORDER = "AddOrder";
		public static final String PRIVILEGE_UPDATE_ORDER = "UpdateOrder";
		public static final String PRIVILEGE_REMOVE_ORDER = "RemoveOrder";

		public static final String PRIVILEGE_ADD_ACTIVITY = "AddActivity";
		public static final String PRIVILEGE_UPDATE_ACTIVITY = "UpdateActivity";
		public static final String PRIVILEGE_REMOVE_ACTIVITY = "RemoveActivity";
		
		public static final String PRIVILEGE_GET_PREFIX= "Get";

		public static final String INTERNAL = StrolchModelConstants.INTERNAL;
	}

	public static class PolicyConstants {
		public static final String TYPE_PRODUCE = "Produce";
		public static final String TYPE_CONSUME = "Consume";

		public static final String BAG_OBJECTIVES = "Objectives";

		public static final String PARAM_QUANTITY = "quantity";
		public static final String PARAM_DURATION = "duration";
	}
}
