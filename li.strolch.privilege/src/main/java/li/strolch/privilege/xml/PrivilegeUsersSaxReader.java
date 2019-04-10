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
package li.strolch.privilege.xml;

import java.text.MessageFormat;
import java.util.*;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeUsersSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeUsersSaxReader.class);

	private Deque<ElementParser> buildersStack = new ArrayDeque<>();

	private List<User> users;

	public PrivilegeUsersSaxReader() {
		this.users = new ArrayList<>();
	}

	/**
	 * @return the users
	 */
	public List<User> getUsers() {
		return this.users;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (qName.equals(XmlConstants.XML_USER)) {
			this.buildersStack.push(new UserParser());
		} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
			this.buildersStack.push(new PropertyParser());
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
		if (qName.equals(XmlConstants.XML_USER)) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
			elementParser = this.buildersStack.pop();
		}

		if (!this.buildersStack.isEmpty() && elementParser != null)
			this.buildersStack.peek().notifyChild(elementParser);
	}

//	<User userId="1" username="admin" password="8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918">
//	  <Firstname>Application</Firstname>
//	  <Lastname>Administrator</Lastname>
//	  <State>ENABLED</State>
//	  <Locale>en-GB</Locale>
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
		byte[] password;
		byte[] salt;
		String hashAlgorithm;
		int hashIterations = -1;
		int hashKeyLength = -1;
		String firstName;
		String lastname;
		UserState userState;
		Locale locale;
		Set<String> userRoles;
		Map<String, String> parameters;

		public UserParser() {
			this.userRoles = new HashSet<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes)
				throws SAXException {

			this.text = new StringBuilder();

			if (qName.equals(XmlConstants.XML_USER)) {
				this.userId = attributes.getValue(XmlConstants.XML_ATTR_USER_ID);
				this.username = attributes.getValue(XmlConstants.XML_ATTR_USERNAME);

				String password = attributes.getValue(XmlConstants.XML_ATTR_PASSWORD);
				String salt = attributes.getValue(XmlConstants.XML_ATTR_SALT);
				parsePassword(password, salt);
			}
		}

		private void parsePassword(String passwordS, String salt) {

			if (StringHelper.isNotEmpty(salt))
				this.salt = StringHelper.fromHexString(salt);

			if (StringHelper.isEmpty(passwordS))
				return;

			if (!passwordS.startsWith("$")) {
				this.password = StringHelper.fromHexString(passwordS);
			} else {

				String[] parts = passwordS.split("\\$");
				if (parts.length != 4) {
					logger.error("Illegal password " + passwordS + ": Starts with $, but does not have 3 parts!");
				} else {

					String hashAlgorithm = parts[1];
					String[] hashParts = hashAlgorithm.split(",");

					if (hashParts.length != 3) {
						logger.error("Illegal password " + passwordS
								+ ": hashAlgorithm part does not have 3 parts separated by comma!");
					} else {

						this.hashAlgorithm = hashParts[0];
						this.hashIterations = Integer.parseInt(hashParts[1]);
						this.hashKeyLength = Integer.parseInt(hashParts[2]);

						this.salt = StringHelper.fromHexString(parts[2]);
						this.password = StringHelper.fromHexString(parts[3]);
					}
				}
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {

			switch (qName) {
			case XmlConstants.XML_FIRSTNAME:

				this.firstName = this.text.toString().trim();
				break;

			case XmlConstants.XML_LASTNAME:

				this.lastname = this.text.toString().trim();
				break;

			case XmlConstants.XML_STATE:

				this.userState = UserState.valueOf(this.text.toString().trim());
				break;

			case XmlConstants.XML_LOCALE:

				this.locale = Locale.forLanguageTag(this.text.toString().trim());
				break;

			case XmlConstants.XML_ROLE:

				this.userRoles.add(this.text.toString().trim());
				break;

			case XmlConstants.XML_USER:

				User user = new User(this.userId, this.username, this.password, this.salt, this.hashAlgorithm,
						hashIterations, hashKeyLength, this.firstName, this.lastname, this.userState, this.userRoles,
						this.locale, this.parameters);
				logger.info(MessageFormat.format("New User: {0}", user)); //$NON-NLS-1$

				getUsers().add(user);
				break;

			default:

				if (!(qName.equals(XmlConstants.XML_ROLES) //
						|| qName.equals(XmlConstants.XML_PARAMETER) //
						|| qName.equals(XmlConstants.XML_PARAMETERS))) {
					throw new IllegalArgumentException("Unhandled tag " + qName);
				}

				break;
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

		// <Property name="organizationalUnit" value="Development" />

		public Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes)
				throws SAXException {

			switch (qName) {
			case XmlConstants.XML_PROPERTY:

				String key = attributes.getValue(XmlConstants.XML_ATTR_NAME);
				String value = attributes.getValue(XmlConstants.XML_ATTR_VALUE);
				this.parameterMap.put(key, value);
				break;

			default:

				if (!qName.equals(XmlConstants.XML_PROPERTIES)) {
					throw new IllegalArgumentException("Unhandled tag " + qName);
				}
			}
		}

		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}
}
