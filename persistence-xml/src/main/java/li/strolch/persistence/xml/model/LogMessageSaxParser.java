/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import li.strolch.model.log.LogMessage;
import li.strolch.xmlpers.api.SaxParser;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LogMessageSaxParser implements SaxParser<LogMessage> {

	private LogMessage logMessage;

	@Override
	public LogMessage getObject() {
		return this.logMessage;
	}

	@Override
	public void setObject(LogMessage logMessage) {
		this.logMessage = logMessage;
	}

	@Override
	public DefaultHandler getDefaultHandler() {
		return new LogMessageSaxReader(l -> this.logMessage = l);
	}

	@Override
	public void write(XMLStreamWriter xmlWriter) {
		new LogMessageToSaxWriterVisitor(xmlWriter).visit(this.logMessage);
	}
}
