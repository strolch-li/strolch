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

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.PasswordCrypt;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.*;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;
import static li.strolch.utils.helper.StringHelper.toHexString;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeUsersSaxWriter {

	private final List<User> users;
	private final File modelFile;

	public PrivilegeUsersSaxWriter(List<User> users, File modelFile) {
		this.users = users;
		this.modelFile = modelFile;
	}

	public void write() throws IOException, XMLStreamException {

		try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.modelFile), StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(USERS);

			List<User> users = new ArrayList<>(this.users);
			users.sort(comparing(u -> u.getUsername().toLowerCase(Locale.ROOT)));
			for (User user : this.users) {

				// start the user element
				xmlWriter.writeStartElement(USER);

				xmlWriter.writeAttribute(ATTR_USER_ID, user.getUserId());
				xmlWriter.writeAttribute(ATTR_USERNAME, user.getUsername());
				writePassword(user, xmlWriter);

				// add first name element
				if (isNotEmpty(user.getFirstname()))
					writeStringElement(xmlWriter, FIRSTNAME, user.getFirstname());

				// add last name element
				if (isNotEmpty(user.getLastname()))
					writeStringElement(xmlWriter, LASTNAME, user.getLastname());

				// add state element
				writeStringElement(xmlWriter, STATE, user.getUserState().toString());

				// add locale element
				writeStringElement(xmlWriter, LOCALE, user.getLocale().toLanguageTag());

				// add password change requested element
				if (user.isPasswordChangeRequested())
					writeStringElement(xmlWriter, PASSWORD_CHANGE_REQUESTED, "true");

				// only right groups and roles if not a remote user
				if (user.getUserState() != UserState.REMOTE) {

					// add all the group elements
					if (!user.getGroups().isEmpty()) {
						xmlWriter.writeStartElement(GROUPS);
						writeStringList(xmlWriter, GROUP, user.getGroups());
						xmlWriter.writeEndElement();
					}

					// add all the role elements
					if (!user.getRoles().isEmpty()) {
						xmlWriter.writeStartElement(ROLES);
						writeStringList(xmlWriter, ROLE, user.getRoles());
						xmlWriter.writeEndElement();
					}

					// add the parameters
					Map<String, String> properties = user.getProperties();
					if (!properties.isEmpty())
						writeStringMapElement(xmlWriter, properties, PROPERTIES, PROPERTY);
				}

				if (!user.isHistoryEmpty()) {
					UserHistory history = user.getHistory();
					xmlWriter.writeStartElement(HISTORY);

					if (!history.isFirstLoginEmpty())
						writeStringElement(xmlWriter, FIRST_LOGIN, ISO8601.toString(history.getFirstLogin()));

					if (!history.isLastLoginEmpty())
						writeStringElement(xmlWriter, LAST_LOGIN, ISO8601.toString(history.getLastLogin()));

					if (!history.isLastPasswordChangeEmpty())
						writeStringElement(xmlWriter, LAST_PASSWORD_CHANGE,
								ISO8601.toString(history.getLastPasswordChange()));

					xmlWriter.writeEndElement();
				}

				xmlWriter.writeEndElement();
			}

			// and now end
			xmlWriter.writeEndDocument();
			xmlWriter.flush();
		}
	}

	private void writePassword(User user, XMLStreamWriter xmlStreamWriter) throws XMLStreamException {
		PasswordCrypt passwordCrypt = user.getPasswordCrypt();
		if (passwordCrypt == null)
			return;

		String passwordString = passwordCrypt.buildPasswordString();
		if (passwordString != null) {
			xmlStreamWriter.writeAttribute(ATTR_PASSWORD, passwordString);
		} else {
			if (passwordCrypt.password() != null)
				xmlStreamWriter.writeAttribute(ATTR_PASSWORD, toHexString(passwordCrypt.password()));
			if (passwordCrypt.salt() != null)
				xmlStreamWriter.writeAttribute(ATTR_SALT, toHexString(passwordCrypt.salt()));
		}
	}
}
