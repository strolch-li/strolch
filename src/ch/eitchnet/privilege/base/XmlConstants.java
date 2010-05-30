/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.base;

/**
 * @author rvonburg
 * 
 */
public class XmlConstants {
	public static final String XML_HANDLER_ENCRYPTION = "EncryptionHandler";
	public static final String XML_HANDLER_SESSION = "SessionHandler";
	public static final String XML_HANDLER_POLICY = "PolicyHandler";

	public static final String XML_ROLES = "Roles";
	public static final String XML_ROLE = "role";
	public static final String XML_USER = "User";
	public static final String XML_PRIVILEGE = "Privilege";
	public static final String XML_POLICY = "Policy";
	public static final String XML_PARAMETERS = "Parameters";
	public static final String XML_ALL_ALLOWED = "allAllowed";
	public static final String XML_DENY = "deny";
	public static final String XML_ALLOW = "allow";
	public static final String XML_FIRSTNAME = "firstname";
	public static final String XML_SURNAME = "surname";
	public static final String XML_STATE = "state";
	public static final String XML_LOCALE = "locale";

	public static final String XML_ATTR_CLASS = "class";
	public static final String XML_ATTR_NAME = "name";
	public static final String XML_ATTR_POLICY = "policy";
	public static final String XML_ATTR_USERNAME = "username";
	public static final String XML_ATTR_PASSWORD = "password";

	public static final String XML_PARAM_HASH_ALGORITHM = "hashAlgorithm";
	public static final String XML_PARAM_POLICY_FILE = "policyXmlFile";
	public static final String XML_PARAM_ROLES_FILE = "rolesXmlFile";
	public static final String XML_PARAM_USERS_FILE = "usersXmlFile";
}
