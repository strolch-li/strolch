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

import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.User;

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
				this.password = attributes.getValue(XmlConstants.XML_ATTR_PASSWORD);
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {

			if (qName.equals(XmlConstants.XML_FIRSTNAME)) {
				this.firstName = this.text.toString().trim();
			} else if (qName.equals(XmlConstants.XML_LASTNAME)) {
				this.lastname = this.text.toString().trim();
			} else if (qName.equals(XmlConstants.XML_STATE)) {
				this.userState = UserState.valueOf(this.text.toString().trim());
			} else if (qName.equals(XmlConstants.XML_LOCALE)) {
				this.locale = new Locale(this.text.toString().trim());
			} else if (qName.equals(XmlConstants.XML_ROLE)) {
				this.userRoles.add(this.text.toString().trim());
			} else if (qName.equals(XmlConstants.XML_ROLES)) {
				// NO-OP
			} else if (qName.equals(XmlConstants.XML_PARAMETER)) {
				// NO-OP
			} else if (qName.equals(XmlConstants.XML_PARAMETERS)) {
				// NO-OP
			} else if (qName.equals(XmlConstants.XML_USER)) {

				User user = new User(this.userId, this.username, this.password, this.firstName, this.lastname,
						this.userState, this.userRoles, this.locale, this.parameters);
				logger.info(MessageFormat.format("New User: {0}", user)); //$NON-NLS-1$
				getUsers().add(user);
			} else {
				throw new IllegalArgumentException("Unhandled tag " + qName);
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

		public Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes)
				throws SAXException {
			if (qName.equals(XmlConstants.XML_PROPERTY)) {
				String key = attributes.getValue(XmlConstants.XML_ATTR_NAME);
				String value = attributes.getValue(XmlConstants.XML_ATTR_VALUE);
				this.parameterMap.put(key, value);
			} else if (qName.equals(XmlConstants.XML_PROPERTIES)) {
				// NO-OP
			} else {
				throw new IllegalArgumentException("Unhandled tag " + qName);
			}
		}

		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}
}
