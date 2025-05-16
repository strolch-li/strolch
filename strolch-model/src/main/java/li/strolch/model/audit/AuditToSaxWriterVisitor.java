/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.text.MessageFormat;

import static li.strolch.model.Tags.Audit.*;

public class AuditToSaxWriterVisitor implements AuditVisitor<Void> {

	protected final XMLStreamWriter writer;

	public AuditToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	@Override
	public Void visitAudit(Audit audit) {

		try {
			writeElement(audit);
			this.writer.flush();
		} catch (XMLStreamException e) {
			String msg = "Failed to write Audit {0} due to {1}";
			msg = MessageFormat.format(msg, audit.getId(), e.getMessage());
			throw new StrolchException(msg, e);
		}

		return null;
	}

	private void writeElement(Audit audit) throws XMLStreamException {

		this.writer.writeStartElement(Tags.AUDIT);
		this.writer.writeAttribute(ID, audit.getId().toString());

		writeElem(USERNAME, audit.getUsername());

		writeElem(DATE, ISO8601.toString(audit.getDate()));

		writeElem(ELEMENT_TYPE, audit.getElementType());
		writeElem(ELEMENT_SUB_TYPE, audit.getElementSubType());
		writeElem(ELEMENT_ACCESSED, audit.getElementAccessed());

		if (audit.getNewVersion() != null)
			writeElem(NEW_VERSION, ISO8601.toString(audit.getNewVersion()));

		writeElem(ACTION, audit.getAction());
		writeElem(ACCESS_TYPE, audit.getAccessType().name());

		if (audit.getAdditionalData() != null) {
			Gson gson = new GsonBuilder().setPrettyPrinting().create();
			writeElem(ADDITIONAL_DATA, gson.toJson(audit.getAdditionalData()), true);
		}

		this.writer.writeEndElement();
	}

	private void writeElem(String tag, String text) throws XMLStreamException {
		writeElem(tag, text, false);
	}

	private void writeElem(String tag, String text, boolean cdata) throws XMLStreamException {
		if (StringHelper.isEmpty(text))
			return;

		this.writer.writeStartElement(tag);
		if (cdata)
			this.writer.writeCData(text);
		else
			this.writer.writeCharacters(text);
		this.writer.writeEndElement();
	}
}
