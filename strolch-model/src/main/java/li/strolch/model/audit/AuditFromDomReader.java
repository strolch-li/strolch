/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import com.google.gson.JsonParser;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.text.MessageFormat;

import static li.strolch.model.Tags.Audit.*;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditFromDomReader {

	public Audit from(Element rootElement) {

		Audit audit = new Audit();

		String idS = rootElement.getAttribute(ID);
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
				case USERNAME -> audit.setUsername(txtContent);
				case DATE -> audit.setDate(ISO8601.parseToDate(txtContent));
				case ELEMENT_TYPE -> audit.setElementType(txtContent);
				case ELEMENT_SUB_TYPE -> audit.setElementSubType(txtContent);
				case ELEMENT_ACCESSED -> audit.setElementAccessed(txtContent);
				case NEW_VERSION -> audit.setNewVersion(ISO8601.parseToDate(txtContent));
				case ACTION -> audit.setAction(txtContent);
				case ACCESS_TYPE -> audit.setAccessType(AccessType.valueOf(txtContent));
				case ADDITIONAL_DATA -> {
					if (isNotEmpty(txtContent))
						audit.setAdditionalData(JsonParser.parseString(txtContent));
				}
				default -> throw new IllegalArgumentException(
						MessageFormat.format("Unhandled/Invalid tag {0} for Audit {1}", nodeName, idS));
			}
		}

		String msg = " missing for element with id " + audit.getId();
		DBC.INTERIM.assertNotEmpty("Username" + msg, audit.getUsername());
		DBC.INTERIM.assertNotNull("Date" + msg, audit.getDate());
		DBC.INTERIM.assertNotEmpty("ElementType" + msg, audit.getElementType());
		DBC.INTERIM.assertNotEmpty("ElementSubType" + msg, audit.getElementSubType());
		DBC.INTERIM.assertNotEmpty("ElementAccessed" + msg, audit.getElementAccessed());
		DBC.INTERIM.assertNotEmpty("Action" + msg, audit.getAction());
		DBC.INTERIM.assertNotNull("AccessType" + msg, audit.getAccessType());

		return audit;
	}
}
