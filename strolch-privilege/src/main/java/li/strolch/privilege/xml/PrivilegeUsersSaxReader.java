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

import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.PasswordCrypt;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.*;

import static li.strolch.privilege.helper.XmlConstants.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeUsersSaxReader extends DefaultHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeUsersSaxReader.class);

	private final Deque<ElementParser> buildersStack = new ArrayDeque<>();

	private final Map<String, User> users;
	private final boolean caseInsensitiveUsername;

	public PrivilegeUsersSaxReader(boolean caseInsensitiveUsername) {
		this.caseInsensitiveUsername = caseInsensitiveUsername;
		this.users = new HashMap<>();
	}

	/**
	 * @return the users
	 */
	public Map<String, User> getUsers() {
		return this.users;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
		if (qName.equals(USER)) {
			if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(UserParser.class)))
				throw new IllegalArgumentException("Previous User not closed!");
			this.buildersStack.push(new UserParser());
		} else if (qName.equals(PROPERTIES)) {
			if (this.buildersStack.stream().anyMatch(e -> e.getClass().equals(PropertyParser.class)))
				throw new IllegalArgumentException("Previous Properties not closed!");
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
		if (qName.equals(USER)) {
			elementParser = this.buildersStack.pop();
		} else if (qName.equals(PROPERTIES)) {
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
		PasswordCrypt passwordCrypt;
		String firstName;
		String lastname;
		UserState userState;
		Locale locale;
		final Set<String> groups;
		final Set<String> userRoles;
		Map<String, String> parameters;
		UserHistory history;
		boolean passwordChangeRequested;

		public UserParser() {
			this.groups = new HashSet<>();
			this.userRoles = new HashSet<>();
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attributes) {

			this.text = new StringBuilder();

			if (qName.equals(USER)) {
				this.userId = attributes.getValue(ATTR_USER_ID).trim();
				this.username = attributes.getValue(ATTR_USERNAME).trim();

				String password = attributes.getValue(ATTR_PASSWORD);
				String salt = attributes.getValue(ATTR_SALT);
				this.passwordCrypt = PasswordCrypt.parse(password, salt);
			} else if (qName.equals(HISTORY)) {
				this.history = UserHistory.EMPTY;
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) {
			this.text.append(ch, start, length);
		}

		@Override
		public void endElement(String uri, String localName, String qName) {

			switch (qName) {
				case FIRSTNAME -> this.firstName = getText();
				case LASTNAME -> this.lastname = getText();
				case STATE -> this.userState = UserState.valueOf(getText());
				case LOCALE -> this.locale = Locale.forLanguageTag(getText());
				case PASSWORD_CHANGE_REQUESTED -> this.passwordChangeRequested = Boolean.parseBoolean(getText());
				case FIRST_LOGIN -> this.history = this.history.withFirstLogin(ISO8601.parseToZdt(getText()));
				case LAST_LOGIN -> this.history = this.history.withLastLogin(ISO8601.parseToZdt(getText()));
				case LAST_PASSWORD_CHANGE ->
						this.history = this.history.withLastPasswordChange(ISO8601.parseToZdt(getText()));
				case GROUP -> this.groups.add(getText());
				case ROLE -> this.userRoles.add(getText());
				case USER -> {
					if (this.history == null)
						this.history = UserHistory.EMPTY;

					User user = new User(this.userId, this.username, this.passwordCrypt, this.firstName, this.lastname,
							this.userState, this.groups, this.userRoles, this.locale, this.parameters,
							this.passwordChangeRequested, this.history);

					logger.info("New User: {}", user);
					String username = caseInsensitiveUsername ? user.getUsername().toLowerCase() : user.getUsername();
					users.put(username, user);
				}
				default -> {
					if (!(
							qName.equals(ROLES) || qName.equals(GROUPS) || qName.equals(PARAMETER) || qName.equals(
									HISTORY) || qName.equals(PARAMETERS))) {
						throw new IllegalArgumentException("Unhandled tag " + qName);
					}
				}
			}
		}

		private String getText() {
			return this.text.toString().trim();
		}

		@Override
		public void notifyChild(ElementParser child) {
			if (child instanceof PropertyParser) {
				this.parameters = ((PropertyParser) child).parameterMap;
			}
		}
	}
}
