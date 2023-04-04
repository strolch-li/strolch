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

import static li.strolch.privilege.helper.XmlConstants.*;

import java.text.MessageFormat;
import java.util.*;

import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
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

	private final Deque<ElementParser> buildersStack = new ArrayDeque<>();

	private final List<User> users;

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
		if (qName.equals(XML_USER)) {
			this.buildersStack.push(new UserParser());
		} else if (qName.equals(XML_PROPERTIES)) {
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
		if (qName.equals(XML_USER)) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals(XML_PROPERTIES)) {
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
//    <History>
//      <FirstLogin>2021-02-19T15:32:09.592+01:00</FirstLogin>
//      <LastLogin>2021-02-19T15:32:09.592+01:00</LastLogin>
//      <LastPasswordChange>2021-02-19T15:32:09.592+01:00</LastPasswordChange>
//    </History>
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
		UserHistory history;
		boolean passwordChangeRequested;

		public UserParser() {
			this.userRoles = new HashSet<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			this.text = new StringBuilder();

			if (qName.equals(XML_USER)) {
				this.userId = attributes.getValue(XML_ATTR_USER_ID).trim();
				this.username = attributes.getValue(XML_ATTR_USERNAME).trim();

				String password = attributes.getValue(XML_ATTR_PASSWORD);
				String salt = attributes.getValue(XML_ATTR_SALT);
				parsePassword(password, salt);
			} else if (qName.equals(XML_HISTORY)) {
				this.history = new UserHistory();
			}
		}

		private void parsePassword(String passwordS, String salt) {

			if (StringHelper.isNotEmpty(salt))
				this.salt = StringHelper.fromHexString(salt.trim());

			if (StringHelper.isEmpty(passwordS))
				return;

			passwordS = passwordS.trim();

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
		public void characters(char[] ch, int start, int length) {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) {

			switch (qName) {
			case XML_FIRSTNAME -> this.firstName = this.text.toString().trim();
			case XML_LASTNAME -> this.lastname = this.text.toString().trim();
			case XML_STATE -> this.userState = UserState.valueOf(this.text.toString().trim());
			case XML_LOCALE -> this.locale = Locale.forLanguageTag(this.text.toString().trim());
			case XML_PASSWORD_CHANGE_REQUESTED ->
					this.passwordChangeRequested = Boolean.parseBoolean(this.text.toString().trim());
			case XML_FIRST_LOGIN -> this.history.setFirstLogin(ISO8601.parseToZdt(this.text.toString().trim()));
			case XML_LAST_LOGIN -> this.history.setLastLogin(ISO8601.parseToZdt(this.text.toString().trim()));
			case XML_LAST_PASSWORD_CHANGE ->
					this.history.setLastPasswordChange(ISO8601.parseToZdt(this.text.toString().trim()));
			case XML_ROLE -> this.userRoles.add(this.text.toString().trim());
			case XML_USER -> {
				if (this.history == null)
					this.history = new UserHistory();
				User user = new User(this.userId, this.username, this.password, this.salt, this.hashAlgorithm,
						hashIterations, hashKeyLength, this.firstName, this.lastname, this.userState, this.userRoles,
						this.locale, this.parameters, this.passwordChangeRequested, this.history);
				logger.info(MessageFormat.format("New User: {0}", user));
				getUsers().add(user);
			}
			default -> {
				if (!(qName.equals(XML_ROLES) //
						|| qName.equals(XML_PARAMETER) //
						|| qName.equals(XML_HISTORY) //
						|| qName.equals(XML_PARAMETERS))) {
					throw new IllegalArgumentException("Unhandled tag " + qName);
				}
			}
			}
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (child instanceof PropertyParser) {
				this.parameters = ((PropertyParser) child).parameterMap;
			}
		}
	}

	static class PropertyParser extends ElementParserAdapter {

		// <Property name="organizationalUnit" value="Development" />

		public Map<String, String> parameterMap = new HashMap<>();

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			switch (qName) {
			case XML_PROPERTY -> {
				String key = attributes.getValue(XML_ATTR_NAME).trim();
				String value = attributes.getValue(XML_ATTR_VALUE).trim();
				this.parameterMap.put(key, value);
			}
			default -> {
				if (!qName.equals(XML_PROPERTIES)) {
					throw new IllegalArgumentException("Unhandled tag " + qName);
				}
			}
			}
		}

		public Map<String, String> getParameterMap() {
			return this.parameterMap;
		}
	}
}
