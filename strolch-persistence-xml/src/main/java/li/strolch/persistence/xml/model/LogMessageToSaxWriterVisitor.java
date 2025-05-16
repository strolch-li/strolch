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

package li.strolch.persistence.xml.model;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.model.log.LogMessage;
import li.strolch.utils.iso8601.ISO8601;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.text.MessageFormat;
import java.util.Properties;
import java.util.Set;

import static li.strolch.utils.helper.StringHelper.isEmpty;

public class LogMessageToSaxWriterVisitor {

	protected final XMLStreamWriter writer;

	public LogMessageToSaxWriterVisitor(XMLStreamWriter writer) {
		this.writer = writer;
	}

	public void visit(LogMessage logMessage) {

		try {
			writeElement(logMessage);
			this.writer.flush();
		} catch (XMLStreamException e) {
			String msg = "Failed to write LogMessage {0} due to {1}";
			msg = MessageFormat.format(msg, logMessage.getId(), e.getMessage());
			throw new StrolchException(msg, e);
		}
	}

	private void writeElement(LogMessage logMessage) throws XMLStreamException {

		this.writer.writeStartElement(Tags.LOG_MESSAGE);
		this.writer.writeAttribute(Tags.ID, logMessage.getId());
		this.writer.writeAttribute(Tags.REALM, logMessage.getRealm());
		this.writer.writeAttribute(Tags.DATE, ISO8601.toString(logMessage.getZonedDateTime()));

		writeElem(Tags.USERNAME, logMessage.getUsername());

		writeElem(Tags.MESSAGE, logMessage.getMessage());
		writeElem(Tags.SEVERITY, logMessage.getSeverity().name());
		writeElem(Tags.STATE, logMessage.getState().name());
		writeElem(Tags.USERNAME, logMessage.getUsername());
		writeElem(Tags.LOCATOR, logMessage.getLocator().toString());
		writeElem(Tags.BUNDLE, logMessage.getBundle());
		writeElem(Tags.KEY, logMessage.getKey());
		writeElem(Tags.EXCEPTION, logMessage.getStackTrace());

		Properties properties = logMessage.getValues();
		Set<String> keys = properties.stringPropertyNames();
		if (!keys.isEmpty()) {
			this.writer.writeStartElement(Tags.PROPERTIES);

			for (String key : keys) {
				this.writer.writeStartElement(Tags.PROPERTY);
				this.writer.writeAttribute(Tags.KEY, key);
				this.writer.writeAttribute(Tags.VALUE, properties.getProperty(key));
				this.writer.writeEndElement();
			}

			this.writer.writeEndElement();
		}

		this.writer.writeEndElement();
	}

	private void writeElem(String tag, String text) throws XMLStreamException {
		if (isEmpty(text))
			return;

		this.writer.writeStartElement(tag);
		this.writer.writeCharacters(text);
		this.writer.writeEndElement();
	}
}
