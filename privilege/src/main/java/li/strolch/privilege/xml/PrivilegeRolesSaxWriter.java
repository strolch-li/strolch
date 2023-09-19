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
import li.strolch.privilege.model.IPrivilege;
import li.strolch.privilege.model.internal.Role;

import javax.xml.stream.XMLStreamException;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeRolesSaxWriter {

	private final List<Role> roles;
	private final File modelFile;

	public PrivilegeRolesSaxWriter(List<Role> roles, File modelFile) {
		this.roles = roles;
		this.modelFile = modelFile;
	}

	public void write() throws IOException, XMLStreamException {

		try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.modelFile), StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(XML_ROLES);

			List<Role> roles = new ArrayList<>(this.roles);
			roles.sort(comparing(role1 -> role1.getName().toLowerCase(Locale.ROOT)));
			for (Role role : roles) {

				// start the role element
				xmlWriter.writeStartElement(XML_ROLE);
				xmlWriter.writeAttribute(XML_ATTR_NAME, role.getName());

				List<String> privilegeNames = new ArrayList<>(role.getPrivilegeNames());
				privilegeNames.sort(null);
				for (String privilegeName : privilegeNames) {
					IPrivilege privilege = role.getPrivilege(privilegeName);

					xmlWriter.writeStartElement(XML_PRIVILEGE);
					xmlWriter.writeAttribute(XML_ATTR_NAME, privilege.getName());
					xmlWriter.writeAttribute(XML_ATTR_POLICY, privilege.getPolicy());

					if (privilege.isAllAllowed())
						writeStringElement(xmlWriter, XML_ALL_ALLOWED, "true");
					writeStringList(xmlWriter, XML_DENY, privilege.getDenyList());
					writeStringList(xmlWriter, XML_ALLOW, privilege.getAllowList());

					xmlWriter.writeEndElement();
				}

				xmlWriter.writeEndElement();
			}

			// and now end
			xmlWriter.writeEndDocument();
			xmlWriter.flush();
		}
	}
}
