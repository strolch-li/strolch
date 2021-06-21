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

import static java.util.Comparator.comparing;

import java.io.File;
import java.util.List;
import java.util.Locale;

import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.internal.Role;
import li.strolch.utils.helper.XmlHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRolesDomWriter {

	private final List<Role> roles;
	private final File modelFile;

	public PrivilegeRolesDomWriter(List<Role> roles, File modelFile) {
		this.roles = roles;
		this.modelFile = modelFile;
	}

	public void write() {

		// create document root
		Document doc = XmlHelper.createDocument();
		Element rootElement = doc.createElement(XmlConstants.XML_ROLES);
		doc.appendChild(rootElement);

		this.roles.stream().sorted(comparing(role1 -> role1.getName().toLowerCase(Locale.ROOT))).forEach(role -> {

			// create the role element
			Element roleElement = doc.createElement(XmlConstants.XML_ROLE);
			rootElement.appendChild(roleElement);

			roleElement.setAttribute(XmlConstants.XML_ATTR_NAME, role.getName());

			role.getPrivilegeNames().stream().sorted().map(role::getPrivilege).forEach(privilege -> {
				Element privilegeElement = doc.createElement(XmlConstants.XML_PRIVILEGE);
				roleElement.appendChild(privilegeElement);
				privilegeElement.setAttribute(XmlConstants.XML_ATTR_NAME, privilege.getName());
				privilegeElement.setAttribute(XmlConstants.XML_ATTR_POLICY, privilege.getPolicy());

				if (privilege.isAllAllowed()) {
					Element allAllowedElement = doc.createElement(XmlConstants.XML_ALL_ALLOWED);
					allAllowedElement.setTextContent(Boolean.toString(privilege.isAllAllowed()));
					privilegeElement.appendChild(allAllowedElement);
				}

				privilege.getDenyList().stream().sorted().forEach(denyValue -> {
					Element denyValueElement = doc.createElement(XmlConstants.XML_DENY);
					denyValueElement.setTextContent(denyValue);
					privilegeElement.appendChild(denyValueElement);
				});

				privilege.getAllowList().stream().sorted().forEach(allowValue -> {
					Element allowValueElement = doc.createElement(XmlConstants.XML_ALLOW);
					allowValueElement.setTextContent(allowValue);
					privilegeElement.appendChild(allowValueElement);
				});
			});
		});

		// write the container file to disk
		XmlHelper.writeDocument(doc, this.modelFile);
	}
}
