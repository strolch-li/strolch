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
import li.strolch.privilege.model.internal.Group;

import javax.xml.stream.XMLStreamException;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static java.util.Comparator.comparing;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.privilege.helper.XmlHelper.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeGroupsSaxWriter {

	private final List<Group> groups;
	private final File modelFile;

	public PrivilegeGroupsSaxWriter(List<Group> groups, File modelFile) {
		this.groups = groups;
		this.modelFile = modelFile;
	}

	public void write() throws IOException, XMLStreamException {

		try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.modelFile), StandardCharsets.UTF_8)) {

			IndentingXMLStreamWriter xmlWriter = openXmlStreamWriterDocument(ioWriter);
			xmlWriter.writeStartElement(GROUPS);

			List<Group> groups = new ArrayList<>(this.groups);
			groups.sort(comparing(g -> g.name().toLowerCase(Locale.ROOT)));
			for (Group group : this.groups) {

				// start the user element
				xmlWriter.writeStartElement(GROUP);

				xmlWriter.writeAttribute(ATTR_NAME, group.name());

				// add all the role elements
				if (!group.roles().isEmpty()) {
					xmlWriter.writeStartElement(ROLES);
					writeStringList(xmlWriter, ROLE, group.roles());
					xmlWriter.writeEndElement();
				}

				// add the parameters
				Map<String, String> properties = group.getProperties();
				if (!properties.isEmpty())
					writeStringMapElement(xmlWriter, properties, PROPERTIES, PROPERTY);

				xmlWriter.writeEndElement();
			}

			// and now end
			xmlWriter.writeEndDocument();
			xmlWriter.flush();
		}
	}
}
