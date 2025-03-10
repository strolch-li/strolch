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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import li.strolch.model.Tags;
import li.strolch.utils.helper.DomUtil;
import li.strolch.utils.iso8601.ISO8601;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;

import static li.strolch.model.Tags.Audit.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditToDomVisitor implements AuditVisitor<Document> {

	@Override
	public Document visitAudit(Audit audit) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document doc = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element auditE = doc.createElement(Tags.AUDIT);
		auditE.setAttribute(ID, audit.getId().toString());

		auditE.appendChild(elem(doc, USERNAME, audit.getUsername()));

		auditE.appendChild(elem(doc, FIRSTNAME, audit.getFirstname()));
		auditE.appendChild(elem(doc, LASTNAME, audit.getLastname()));
		auditE.appendChild(elem(doc, DATE, ISO8601.toString(audit.getDate())));

		auditE.appendChild(elem(doc, ELEMENT_TYPE, audit.getElementType()));
		auditE.appendChild(elem(doc, ELEMENT_SUB_TYPE, audit.getElementSubType()));
		auditE.appendChild(elem(doc, ELEMENT_ACCESSED, audit.getElementAccessed()));

		if (audit.getNewVersion() != null)
			auditE.appendChild(elem(doc, NEW_VERSION, ISO8601.toString(audit.getNewVersion())));

		auditE.appendChild(elem(doc, ACTION, audit.getAction()));
		auditE.appendChild(elem(doc, ACCESS_TYPE, audit.getAccessType().name()));

		if (audit.getAdditionalData() != null) {
			Gson gson = new GsonBuilder().setPrettyPrinting().create();
			auditE.appendChild(elem(doc, ADDITIONAL_DATA, gson.toJson(audit.getAdditionalData())));
		}

		doc.appendChild(auditE);
		return doc;
	}

	private Element elem(Document doc, String tag, String txtValue) {
		Element element = doc.createElement(tag);
		element.setTextContent(txtValue);
		return element;
	}
}
