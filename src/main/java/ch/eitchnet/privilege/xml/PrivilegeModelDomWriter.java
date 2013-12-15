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

import java.io.File;
import java.util.List;
import java.util.Map.Entry;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.utils.helper.XmlHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class PrivilegeModelDomWriter {

	private List<User> users;
	private List<Role> roles;
	private File modelFile;

	/**
	 * 
	 */
	public PrivilegeModelDomWriter(List<User> users, List<Role> roles, File modelFile) {
		this.users = users;
		this.roles = roles;
		this.modelFile = modelFile;
	}

	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XmlConstants.XML_ROOT_PRIVILEGE_USERS_AND_ROLES);
		doc.appendChild(rootElement);

		Element usersElement = doc.createElement(XmlConstants.XML_USERS);
		rootElement.appendChild(usersElement);

		for (User user : this.users) {

			// create the user element
			Element userElement = doc.createElement(XmlConstants.XML_USER);
			usersElement.appendChild(userElement);

			userElement.setAttribute(XmlConstants.XML_ATTR_USER_ID, user.getUserId());
			userElement.setAttribute(XmlConstants.XML_ATTR_USERNAME, user.getUsername());
			userElement.setAttribute(XmlConstants.XML_ATTR_PASSWORD, user.getPassword());

			// add first name element
			Element firstnameElement = doc.createElement(XmlConstants.XML_FIRSTNAME);
			firstnameElement.setTextContent(user.getFirstname());
			userElement.appendChild(firstnameElement);

			// add surname element
			Element surnameElement = doc.createElement(XmlConstants.XML_SURNAME);
			surnameElement.setTextContent(user.getSurname());
			userElement.appendChild(surnameElement);

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
			Element parametersElement = doc.createElement(XmlConstants.XML_PARAMETERS);
			userElement.appendChild(parametersElement);
			for (Entry<String, String> entry : user.getProperties().entrySet()) {
				Element paramElement = doc.createElement(XmlConstants.XML_PARAMETER);
				paramElement.setAttribute(XmlConstants.XML_ATTR_NAME, entry.getKey());
				paramElement.setAttribute(XmlConstants.XML_ATTR_VALUE, entry.getValue());
				parametersElement.appendChild(paramElement);
			}
		}

		Element rolesElement = doc.createElement(XmlConstants.XML_ROLES);
		rootElement.appendChild(rolesElement);

		for (Role role : this.roles) {

			// create the role element
			Element roleElement = doc.createElement(XmlConstants.XML_ROLE);
			rolesElement.appendChild(roleElement);

			roleElement.setAttribute(XmlConstants.XML_ATTR_NAME, role.getName());

			for (String privilegeName : role.getPrivilegeNames()) {
				IPrivilege privilege = role.getPrivilege(privilegeName);

				// create the privilege element
				Element privilegeElement = doc.createElement(XmlConstants.XML_PRIVILEGE);
				roleElement.appendChild(privilegeElement);

				privilegeElement.setAttribute(XmlConstants.XML_ATTR_NAME, privilege.getName());
				privilegeElement.setAttribute(XmlConstants.XML_ATTR_POLICY, privilege.getPolicy());

				// add the all allowed element
				Element allAllowedElement = doc.createElement(XmlConstants.XML_ALL_ALLOWED);
				allAllowedElement.setTextContent(Boolean.toString(privilege.isAllAllowed()));
				privilegeElement.appendChild(allAllowedElement);

				// add all the deny values
				for (String denyValue : privilege.getDenyList()) {
					Element denyValueElement = doc.createElement(XmlConstants.XML_DENY);
					denyValueElement.setTextContent(denyValue);
					privilegeElement.appendChild(denyValueElement);
				}

				// add all the allow values
				for (String allowValue : privilege.getAllowList()) {
					Element allowValueElement = doc.createElement(XmlConstants.XML_ALLOW);
					allowValueElement.setTextContent(allowValue);
					privilegeElement.appendChild(allowValueElement);
				}
			}
		}

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.modelFile);
	}
}
