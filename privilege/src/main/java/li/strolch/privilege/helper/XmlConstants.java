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

	public static final String ROOT_PRIVILEGE = "Privilege";
	public static final String CONTAINER = "Container";
	public static final String POLICIES = "Policies";
	public static final String PRIVILEGES = "Privileges";
	public static final String ROOT_PRIVILEGE_USERS_AND_ROLES = "UsersAndRoles";
	public static final String ROOT_CERTIFICATES = "Certificates";
	public static final String HANDLER_USER_CHALLENGE = "UserChallengeHandler";
	public static final String HANDLER_PERSISTENCE = "PersistenceHandler";
	public static final String HANDLER_ENCRYPTION = "EncryptionHandler";
	public static final String HANDLER_PASSWORD_STRENGTH = "PasswordStrengthHandler";
	public static final String HANDLER_SSO = "SsoHandler";
	public static final String HANDLER_PRIVILEGE = "PrivilegeHandler";
	public static final String ROLES = "Roles";
	public static final String ROLE = "Role";
	public static final String USERS = "Users";
	public static final String GROUPS = "Groups";
	public static final String GROUP = "Group";
	public static final String CERTIFICATE = "Certificate";
	public static final String SESSION_DATA = "SessionData";
	public static final String USER = "User";
	public static final String HISTORY = "History";
	public static final String FIRST_LOGIN = "FirstLogin";
	public static final String LAST_LOGIN = "LastLogin";
	public static final String LAST_PASSWORD_CHANGE = "LastPasswordChange";
	public static final String PASSWORD_CHANGE_REQUESTED = "PasswordChangeRequested";
	public static final String PRIVILEGE = "Privilege";
	public static final String POLICY = "Policy";
	public static final String PARAMETERS = "Parameters";
	public static final String PARAMETER = "Parameter";
	public static final String PROPERTIES = "Properties";
	public static final String PROPERTY = "Property";
	public static final String ALL_ALLOWED = "AllAllowed";
	public static final String DENY = "Deny";
	public static final String ALLOW = "Allow";
	public static final String FIRSTNAME = "Firstname";
	public static final String LASTNAME = "Lastname";
	public static final String STATE = "State";
	public static final String LOCALE = "Locale";
	public static final String ATTR_CLASS = "class";
	public static final String ATTR_LOGIN_TIME = "loginTime";
	public static final String ATTR_KEEP_ALIVE = "keepAlive";
	public static final String ATTR_LAST_ACCESS = "lastAccess";
	public static final String ATTR_NAME = "name";
	public static final String ATTR_VALUE = "value";
	public static final String ATTR_POLICY = "policy";
	public static final String ATTR_USER_ID = "userId";
	public static final String ATTR_SESSION_ID = "sessionId";
	public static final String ATTR_USAGE = "usage";
	public static final String ATTR_USERNAME = "username";
	public static final String ATTR_AUTH_TOKEN = "authToken";
	public static final String ATTR_SOURCE = "source";
	public static final String ATTR_LOCALE = "locale";
	public static final String ATTR_PASSWORD = "password";
	public static final String ATTR_SALT = "salt";
	public static final String PARAM_HASH_ALGORITHM = "hashAlgorithm";
	public static final String PARAM_HASH_ALGORITHM_NON_SALT = "hashAlgorithmNonSalt";
	public static final String PARAM_HASH_ITERATIONS = "hashIterations";
	public static final String PARAM_HASH_KEY_LENGTH = "hashKeyLength";
	public static final String PARAM_USERS_FILE = "usersXmlFile";
	public static final String PARAM_USERS_FILE_DEF = "PrivilegeUsers.xml";
	public static final String PARAM_GROUPS_FILE = "groupsXmlFile";
	public static final String PARAM_GROUPS_FILE_DEF = "PrivilegeGroups.xml";
	public static final String PARAM_ROLES_FILE = "rolesXmlFile";
	public static final String PARAM_ROLES_FILE_DEF = "PrivilegeRoles.xml";
	public static final String PARAM_BASE_PATH = "basePath";
}
