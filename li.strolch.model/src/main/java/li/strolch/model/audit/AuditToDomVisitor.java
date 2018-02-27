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

import javax.xml.parsers.DocumentBuilder;

import li.strolch.model.Tags;
import li.strolch.utils.helper.DomUtil;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditToDomVisitor implements AuditVisitor<Document> {

	@Override
	public Document visitAudit(Audit audit) {
		DocumentBuilder documentBuilder = DomUtil.createDocumentBuilder();
		Document doc = documentBuilder.getDOMImplementation().createDocument(null, null, null);

		Element auditE = doc.createElement(Tags.AUDIT);
		auditE.setAttribute(Tags.Audit.ID, audit.getId().toString());

		auditE.appendChild(elem(doc, Tags.Audit.USERNAME, audit.getUsername()));

		auditE.appendChild(elem(doc, Tags.Audit.FIRSTNAME, audit.getFirstname()));
		auditE.appendChild(elem(doc, Tags.Audit.LASTNAME, audit.getLastname()));
		auditE.appendChild(elem(doc, Tags.Audit.DATE, ISO8601FormatFactory.getInstance().formatDate(audit.getDate())));

		auditE.appendChild(elem(doc, Tags.Audit.ELEMENT_TYPE, audit.getElementType()));
		auditE.appendChild(elem(doc, Tags.Audit.ELEMENT_SUB_TYPE, audit.getElementSubType()));
		auditE.appendChild(elem(doc, Tags.Audit.ELEMENT_ACCESSED, audit.getElementAccessed()));

		if (audit.getNewVersion() != null)
			auditE.appendChild(elem(doc, Tags.Audit.NEW_VERSION,
					ISO8601FormatFactory.getInstance().formatDate(audit.getNewVersion())));

		auditE.appendChild(elem(doc, Tags.Audit.ACTION, audit.getAction()));
		auditE.appendChild(elem(doc, Tags.Audit.ACCESS_TYPE, audit.getAccessType().name()));

		doc.appendChild(auditE);
		return doc;
	}

	private Element elem(Document doc, String tag, String txtValue) {
		Element element = doc.createElement(tag);
		element.setTextContent(txtValue);
		return element;
	}
}
