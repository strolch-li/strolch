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
package li.strolch.model.audit;

import java.text.MessageFormat;

import li.strolch.model.Tags;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601FormatFactory;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditFromDomReader {

	public Audit from(Element rootElement) {

		Audit audit = new Audit();

		String idS = rootElement.getAttribute(Tags.Audit.ID);
		DBC.INTERIM.assertNotEmpty("Id must be set!", idS);
		audit.setId(Long.parseLong(idS));

		NodeList childNodes = rootElement.getChildNodes();
		for (int i = 0; i < childNodes.getLength(); i++) {
			Node item = childNodes.item(i);
			if (!(item instanceof Element element))
				continue;

			String nodeName = element.getNodeName();
			String txtContent = element.getTextContent();

			switch (nodeName) {
			case Tags.Audit.USERNAME -> audit.setUsername(txtContent);
			case Tags.Audit.FIRSTNAME -> audit.setFirstname(txtContent);
			case Tags.Audit.LASTNAME -> audit.setLastname(txtContent);
			case Tags.Audit.DATE ->
					audit.setDate(ISO8601FormatFactory.getInstance().getXmlDateFormat().parse(txtContent));
			case Tags.Audit.ELEMENT_TYPE -> audit.setElementType(txtContent);
			case Tags.Audit.ELEMENT_SUB_TYPE -> audit.setElementSubType(txtContent);
			case Tags.Audit.ELEMENT_ACCESSED -> audit.setElementAccessed(txtContent);
			case Tags.Audit.NEW_VERSION ->
					audit.setNewVersion(ISO8601FormatFactory.getInstance().getXmlDateFormat().parse(txtContent));
			case Tags.Audit.ACTION -> audit.setAction(txtContent);
			case Tags.Audit.ACCESS_TYPE -> audit.setAccessType(AccessType.valueOf(txtContent));
			default -> throw new IllegalArgumentException(
					MessageFormat.format("Unhandled/Invalid tag {0} for Audit {1}", nodeName, idS));
			}
		}

		String msg = " missing for element with id " + audit.getId();
		DBC.INTERIM.assertNotEmpty("Username" + msg, audit.getUsername());
		DBC.INTERIM.assertNotEmpty("Firstname" + msg, audit.getFirstname());
		DBC.INTERIM.assertNotEmpty("Lastname" + msg, audit.getLastname());
		DBC.INTERIM.assertNotNull("Date" + msg, audit.getDate());
		DBC.INTERIM.assertNotEmpty("ElementType" + msg, audit.getElementType());
		DBC.INTERIM.assertNotEmpty("ElementSubType" + msg, audit.getElementSubType());
		DBC.INTERIM.assertNotEmpty("ElementAccessed" + msg, audit.getElementAccessed());
		//DBC.INTERIM.assertNotNull("NewVersion" + msg, audit.getNewVersion());
		DBC.INTERIM.assertNotEmpty("Action" + msg, audit.getAction());
		DBC.INTERIM.assertNotNull("AccessType" + msg, audit.getAccessType());

		return audit;
	}
}
