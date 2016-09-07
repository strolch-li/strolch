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
package li.strolch.privilege.helper;

/**
 * The constants used in parsing XML documents which contain the configuration for Privilege
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class XmlConstants {

	/**
	 * XML_ROOT_PRIVILEGE_CONTAINER = "PrivilegeContainer" :
	 */
	public static final String XML_ROOT_PRIVILEGE = "Privilege";

	/**
	 * XML_CONTAINER = "Container" :
	 */
	public static final String XML_CONTAINER = "Container";

	/**
	 * XML_POLICIES = "Policies" :
	 */
	public static final String XML_POLICIES = "Policies";

	/**
	 * XML_PRIVILEGES = "Privileges" :
	 */
	public static final String XML_PRIVILEGES = "Privileges";

	/**
	 * XML_ROOT_PRIVILEGE_USERS_AND_ROLES = "UsersAndRoles" :
	 */
	public static final String XML_ROOT_PRIVILEGE_USERS_AND_ROLES = "UsersAndRoles";

	/**
	 * XML_ROOT_CERTIFICATES = "Certificates" :
	 */
	public static final String XML_ROOT_CERTIFICATES = "Certificates";

	/**
	 * XML_HANDLER_USER_CHALLENGE = "UserChallengeHandler" :
	 */
	public static final String XML_HANDLER_USER_CHALLENGE = "UserChallengeHandler";

	/**
	 * XML_HANDLER_PERSISTENCE = "PersistenceHandler" :
	 */
	public static final String XML_HANDLER_PERSISTENCE = "PersistenceHandler";

	/**
	 * XML_HANDLER_ENCRYPTION = "EncryptionHandler" :
	 */
	public static final String XML_HANDLER_ENCRYPTION = "EncryptionHandler";

	/**
	 * XML_HANDLER_PRIVILEGE = "PrivilegeHandler" :
	 */
	public static final String XML_HANDLER_PRIVILEGE = "PrivilegeHandler";

	/**
	 * XML_ROLES = "Roles" :
	 */
	public static final String XML_ROLES = "Roles";

	/**
	 * XML_ROLE = "Role" :
	 */
	public static final String XML_ROLE = "Role";

	/**
	 * XML_USERS = "Users" :
	 */
	public static final String XML_USERS = "Users";

	/**
	 * XML_CERTIFICATE = "Certificate" :
	 */
	public static final String XML_CERTIFICATE = "Certificate";

	/**
	 * XML_SESSION_DATA = "SessionData" :
	 */
	public static final String XML_SESSION_DATA = "SessionData";

	/**
	 * XML_USER = "User"
	 */
	public static final String XML_USER = "User";

	/**
	 * XML_PRIVILEGE = "Privilege" :
	 */
	public static final String XML_PRIVILEGE = "Privilege";

	/**
	 * XML_POLICY = "Policy" :
	 */
	public static final String XML_POLICY = "Policy";

	/**
	 * XML_PARAMETERS = "Parameters" :
	 */
	public static final String XML_PARAMETERS = "Parameters";

	/**
	 * XML_PARAMETER = "Parameter" :
	 */
	public static final String XML_PARAMETER = "Parameter";

	/**
	 * XML_PROPERTIES = "Properties" :
	 */
	public static final String XML_PROPERTIES = "Properties";

	/**
	 * XML_PROPERTY = "Property" :
	 */
	public static final String XML_PROPERTY = "Property";

	/**
	 * XML_ALL_ALLOWED = "AllAllowed" :
	 */
	public static final String XML_ALL_ALLOWED = "AllAllowed";

	/**
	 * XML_DENY = "Deny" :
	 */
	public static final String XML_DENY = "Deny";

	/**
	 * XML_ALLOW = "Allow" :
	 */
	public static final String XML_ALLOW = "Allow";

	/**
	 * XML_FIRSTNAME = "Firstname" :
	 */
	public static final String XML_FIRSTNAME = "Firstname";

	/**
	 * XML_LASTNAME = "Lastname" :
	 */
	public static final String XML_LASTNAME = "Lastname";

	/**
	 * XML_STATE = "State" :
	 */
	public static final String XML_STATE = "State";

	/**
	 * XML_LOCALE = "Locale" :
	 */
	public static final String XML_LOCALE = "Locale";

	/**
	 * XML_ATTR_CLASS = "class" :
	 */
	public static final String XML_ATTR_CLASS = "class";

	/**
	 * XML_ATTR_LOGIN_TIME = "loginTime" :
	 */
	public static final String XML_ATTR_LOGIN_TIME = "loginTime";

	/**
	 * XML_ATTR_LAST_ACCESS = "lastAccess" :
	 */
	public static final String XML_ATTR_LAST_ACCESS = "lastAccess";

	/**
	 * XML_ATTR_NAME = "name" :
	 */
	public static final String XML_ATTR_NAME = "name";

	/**
	 * XML_ATTR_VALUE = "value" :
	 */
	public static final String XML_ATTR_VALUE = "value";

	/**
	 * XML_ATTR_POLICY = "policy" :
	 */
	public static final String XML_ATTR_POLICY = "policy";

	/**
	 * XML_ATTR_USER_ID = "userId" :
	 */
	public static final String XML_ATTR_USER_ID = "userId";

	/**
	 * XML_ATTR_SESSION_ID = "sessionId" :
	 */
	public static final String XML_ATTR_SESSION_ID = "sessionId";

	/**
	 * XML_ATTR_SESSION_ID = "usage" :
	 */
	public static final String XML_ATTR_USAGE = "usage";

	/**
	 * XML_ATTR_USERNAME = "username" :
	 */
	public static final String XML_ATTR_USERNAME = "username";

	/**
	 * XML_ATTR_AUTH_TOKEN = "authToken" :
	 */
	public static final String XML_ATTR_AUTH_TOKEN = "authToken";

	/**
	 * XML_ATTR_LOCALE = "locale" :
	 */
	public static final String XML_ATTR_LOCALE = "locale";

	/**
	 * XML_ATTR_PASSWORD = "password" :
	 */
	public static final String XML_ATTR_PASSWORD = "password";

	/**
	 * XML_PARAM_HASH_ALGORITHM = "hashAlgorithm" :
	 */
	public static final String XML_PARAM_HASH_ALGORITHM = "hashAlgorithm";

	/**
	 * XML_PARAM_USERS_FILE = "usersXmlFile" :
	 */
	public static final String XML_PARAM_USERS_FILE = "usersXmlFile";

	/**
	 * XML_PARAM_ROLES_FILE = "rolesXmlFile" :
	 */
	public static final String XML_PARAM_ROLES_FILE = "rolesXmlFile";

	/**
	 * XML_PARAM_BASE_PATH = "basePath" :
	 */
	public static final String XML_PARAM_BASE_PATH = "basePath";
}
