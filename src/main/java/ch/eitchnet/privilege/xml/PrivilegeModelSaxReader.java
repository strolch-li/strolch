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
package ch.eitchnet.privilege.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.PrivilegeImpl;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeModelSaxReader extends DefaultHandler {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeModelSaxReader.class);

	private Stack<ElementParser> buildersStack = new Stack<ElementParser>();

	private List<User> users;
	private List<Role> roles;

	private boolean insideUser;

	public PrivilegeModelSaxReader() {
		this.users = new ArrayList<User>();
		this.roles = new ArrayList<Role>();
	}

	/**
	 * @return the users
	 */
	public List<User> getUsers() {
		return this.users;
	}

	/**
	 * @return the roles
	 */
	public List<Role> getRoles() {
		return this.roles;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		if (qName.equals("Users")) {
			this.buildersStack.add(new UserParser());
			this.insideUser = true;
		} else if (qName.equals("Properties")) {
			this.buildersStack.add(new PropertyParser());
		} else if (qName.equals("Roles") && !this.insideUser) {
			this.buildersStack.add(new RoleParser());
		}

		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().startElement(uri, localName, qName, attributes);
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().characters(ch, start, length);
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		if (!this.buildersStack.isEmpty())
			this.buildersStack.peek().endElement(uri, localName, qName);

		ElementParser elementParser = null;
		if (qName.equals("Users")) {
			elementParser = this.buildersStack.pop();
			this.insideUser = false;
			PrivilegeModelSaxReader.logger.info("Popping for Users");
		} else if (qName.equals("Properties")) {
			elementParser = this.buildersStack.pop();
			PrivilegeModelSaxReader.logger.info("Popping for Properties");
		} else if (qName.equals("Roles") && !this.insideUser) {
			elementParser = this.buildersStack.pop();
			PrivilegeModelSaxReader.logger.info("Popping for Roles");
		}

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

//	<Role name="AppUser">
//	  <Privilege name="ch.eitchnet.privilege.test.model.TestRestrictable">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	</Role>
//	<Role name="system_admin_privileges">
//	  <Privilege name="ch.eitchnet.privilege.test.model.TestSystemUserAction">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	  <Privilege name="ch.eitchnet.privilege.test.model.TestSystemRestrictable">
//	    <AllAllowed>true</AllAllowed>
//	  </Privilege>
//	</Role>

	public class RoleParser extends ElementParserAdapter {

		private StringBuilder text;

		private String roleName;
		private String privilegeName;
		private String privilegePolicy;
		private boolean allAllowed;
		private Set<String> denyList;
		private Set<String> allowList;

		private Map<String, IPrivilege> privileges;

		/**
		 * 
		 */
		public RoleParser() {
			init();
		}

		/**
		 * 
		 */
		private void init() {
			this.privileges = new HashMap<String, IPrivilege>();

			this.text = null;

			this.roleName = null;
			this.privilegeName = null;
			this.privilegePolicy = null;
			this.allAllowed = false;
			this.denyList = new HashSet<String>();
			this.allowList = new HashSet<String>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

			this.text = new StringBuilder();

			if (qName.equals("Role")) {
				this.roleName = attributes.getValue("name");
			} else if (qName.equals("Privilege")) {
				this.privilegeName = attributes.getValue("name");
				this.privilegePolicy = attributes.getValue("policy");
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (this.text != null)
				this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {

			if (qName.equals("AllAllowed")) {
				this.allAllowed = StringHelper.parseBoolean(this.text.toString().trim());
			} else if (qName.equals("Allow")) {
				this.allowList.add(this.text.toString().trim());
			} else if (qName.equals("Deny")) {
				this.denyList.add(this.text.toString().trim());
			} else if (qName.equals("Privilege")) {

				IPrivilege privilege = new PrivilegeImpl(this.privilegeName, this.privilegePolicy, this.allAllowed,
						this.denyList, this.allowList);
				this.privileges.put(this.privilegeName, privilege);

			} else if (qName.equals("Role")) {

				Role role = new Role(this.roleName, this.privileges);

				PrivilegeModelSaxReader.this.roles.add(role);
				PrivilegeModelSaxReader.logger.info("New Role: " + role);
				init();
			}
		}
	}

//	<User userId="1" username="admin" password="8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918">
//	  <Firstname>Application</Firstname>
//	  <Surname>Administrator</Surname>
//	  <State>ENABLED</State>
//	  <Locale>en_GB</Locale>
//	  <Roles>
//	    <Role>PrivilegeAdmin</Role>
//	    <Role>AppUser</Role>
//	  </Roles>
//	  <Properties>
//	    <Property name="organization" value="eitchnet.ch" />
//	    <Property name="organizationalUnit" value="Development" />
//	  </Properties>
//	</User>

	public class UserParser extends ElementParserAdapter {

		StringBuilder text;

		String userId;
		String username;
		String password;
		String firstName;
		String surname;
		UserState userState;
		Locale locale;
		Set<String> userRoles;
		Map<String, String> parameters;

		public UserParser() {
			this.userRoles = new HashSet<String>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

			this.text = new StringBuilder();

			if (qName.equals("User")) {
				this.userId = attributes.getValue("userId");
				this.username = attributes.getValue("username");
				this.password = attributes.getValue("password");
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {

			if (qName.equals("Firstname")) {
				this.firstName = this.text.toString().trim();
			} else if (qName.equals("Surname")) {
				this.surname = this.text.toString().trim();
			} else if (qName.equals("State")) {
				this.userState = UserState.valueOf(this.text.toString().trim());
			} else if (qName.equals("Locale")) {
				this.locale = Locale.forLanguageTag(this.text.toString().trim());
			} else if (qName.equals("Role")) {
				this.userRoles.add(this.text.toString().trim());
			} else if (qName.equals("User")) {

				User user = new User(this.userId, this.username, this.password, this.firstName, this.surname,
						this.userState, this.userRoles, this.locale, this.parameters);

				PrivilegeModelSaxReader.this.users.add(user);
			}
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (child instanceof PropertyParser) {
				this.parameters = ((PropertyParser) child).parameterMap;
			}
		}
	}

	class PropertyParser extends ElementParserAdapter {

//	      <Property name="organizationalUnit" value="Development" />

		private Map<String, String> parameterMap = new HashMap<String, String>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
			if (qName.equals("Property")) {
				String key = attributes.getValue("name");
				String value = attributes.getValue("value");
				this.parameterMap.put(key, value);
			}
		}

		/**
		 * @return the parameterMap
		 */
		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}
}
