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

import li.strolch.model.Tags;
import li.strolch.utils.iso8601.ISO8601;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.text.MessageFormat;
import java.util.function.Consumer;

import static li.strolch.model.Tags.Audit.*;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditSaxReader extends DefaultHandler {

	private final Consumer<Audit> auditConsumer;

	private Audit currentAudit;
	private StringBuilder sb = new StringBuilder();

	public AuditSaxReader(Consumer<Audit> auditConsumer) {
		this.auditConsumer = auditConsumer;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		switch (qName) {
			case Tags.AUDIT -> {
				this.currentAudit = new Audit();
				this.currentAudit.setId(Long.parseLong(attributes.getValue(ID)));
			}
			case USERNAME, DATE, ELEMENT_TYPE, ELEMENT_SUB_TYPE, ELEMENT_ACCESSED, NEW_VERSION, ACTION, ACCESS_TYPE,
				 ADDITIONAL_DATA -> this.sb = new StringBuilder();
			default -> throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName));
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) {

		switch (qName) {
			case Tags.AUDIT -> {
				this.auditConsumer.accept(this.currentAudit);
				this.currentAudit = null;
			}
			case USERNAME -> {
				this.currentAudit.setUsername(this.sb.toString());
				this.sb = null;
			}
			case DATE -> {
				this.currentAudit.setDate(ISO8601.parseToZdt(this.sb.toString()));
				this.sb = null;
			}
			case ELEMENT_TYPE -> {
				this.currentAudit.setElementType(this.sb.toString());
				this.sb = null;
			}
			case ELEMENT_SUB_TYPE -> {
				this.currentAudit.setElementSubType(this.sb.toString());
				this.sb = null;
			}
			case ELEMENT_ACCESSED -> {
				this.currentAudit.setElementAccessed(this.sb.toString());
				this.sb = null;
			}
			case NEW_VERSION -> {
				this.currentAudit.setNewVersion(ISO8601.parseToZdt(this.sb.toString()));
				this.sb = null;
			}
			case ACTION -> {
				this.currentAudit.setAction(this.sb.toString());
				this.sb = null;
			}
			case ACCESS_TYPE -> {
				this.currentAudit.setAccessType(AccessType.valueOf(this.sb.toString()));
				this.sb = null;
			}
			case ADDITIONAL_DATA -> {
				this.currentAudit.setAdditionalDataAsString(this.sb.toString());
				this.sb = null;
			}
			default -> throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName));
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) {
		if (this.sb != null)
			this.sb.append(ch, start, length);
	}
}
