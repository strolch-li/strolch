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

import java.io.File;
import java.util.Comparator;
import java.util.List;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.internal.User;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.XmlHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeUsersDomWriter {

	private List<User> users;
	private File modelFile;

	public PrivilegeUsersDomWriter(List<User> users, File modelFile) {
		this.users = users;
		this.modelFile = modelFile;
	}

	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XmlConstants.XML_USERS);
		doc.appendChild(rootElement);

		this.users.stream().sorted(Comparator.comparing(User::getUserId)).forEach(user -> {

			// create the user element
			Element userElement = doc.createElement(XmlConstants.XML_USER);
			rootElement.appendChild(userElement);

			userElement.setAttribute(XmlConstants.XML_ATTR_USER_ID, user.getUserId());
			userElement.setAttribute(XmlConstants.XML_ATTR_USERNAME, user.getUsername());
			writePassword(user, userElement);

			// add first name element
			if (StringHelper.isNotEmpty(user.getFirstname())) {
				Element firstnameElement = doc.createElement(XmlConstants.XML_FIRSTNAME);
				firstnameElement.setTextContent(user.getFirstname());
				userElement.appendChild(firstnameElement);
			}

			// add last name element
			if (StringHelper.isNotEmpty(user.getLastname())) {
				Element lastnameElement = doc.createElement(XmlConstants.XML_LASTNAME);
				lastnameElement.setTextContent(user.getLastname());
				userElement.appendChild(lastnameElement);
			}

			// add state element
			Element stateElement = doc.createElement(XmlConstants.XML_STATE);
			stateElement.setTextContent(user.getUserState().toString());
			userElement.appendChild(stateElement);

			// add locale element
			Element localeElement = doc.createElement(XmlConstants.XML_LOCALE);
			localeElement.setTextContent(user.getLocale().toString());
			userElement.appendChild(localeElement);

			// add all the role elements
			Element rolesElement = doc.createElement(XmlConstants.XML_ROLES);
			userElement.appendChild(rolesElement);
			for (String roleName : user.getRoles()) {
				Element roleElement = doc.createElement(XmlConstants.XML_ROLE);
				roleElement.setTextContent(roleName);
				rolesElement.appendChild(roleElement);
			}

			// add the parameters
			if (!user.getProperties().isEmpty()) {
				Element parametersElement = doc.createElement(XmlConstants.XML_PROPERTIES);
				userElement.appendChild(parametersElement);
				for (Entry<String, String> entry : user.getProperties().entrySet()) {
					Element paramElement = doc.createElement(XmlConstants.XML_PROPERTY);
					paramElement.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
					paramElement.setAttribute(XmlConstants.XML_ATTR_VALUE, entry.getValue());
					parametersElement.appendChild(paramElement);
				}
			}
		});

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.modelFile);
	}

	private void writePassword(User user, Element userElement) {

		if (user.getPassword() != null && user.getSalt() != null && user.getHashAlgorithm() != null
				&& user.getHashIterations() != -1 && user.getHashKeyLength() != -1) {

			String algo = user.getHashAlgorithm() + "," + user.getHashIterations() + "," + user.getHashKeyLength();
			String hash = StringHelper.toHexString(user.getSalt());
			String password = StringHelper.toHexString(user.getPassword());

			String passwordS = "$" + algo + "$" + hash + "$" + password;
			userElement.setAttribute(XmlConstants.XML_ATTR_PASSWORD, passwordS);

		} else {

			if (user.getPassword() != null)
				userElement.setAttribute(XmlConstants.XML_ATTR_PASSWORD, StringHelper.toHexString(user.getPassword()));
			if (user.getSalt() != null)
				userElement.setAttribute(XmlConstants.XML_ATTR_SALT, StringHelper.toHexString(user.getSalt()));
		}
	}
}
