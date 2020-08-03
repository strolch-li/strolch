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
package li.strolch.persistence.xml.model;

import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.util.Properties;
import java.util.function.Consumer;

import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.model.Locator;
import li.strolch.model.Tags;
import li.strolch.utils.iso8601.ISO8601;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LogMessageSaxReader extends DefaultHandler {

	private final Consumer<LogMessage> logMessageConsumer;

	private String id;
	private ZonedDateTime dateTime;
	private String realm;
	private String username;
	private Locator locator;
	private LogSeverity severity;
	private LogMessageState state;
	private String bundle;
	private String key;
	private Properties properties;
	private String message;
	private String exception;

	private StringBuilder sb = new StringBuilder();

	public LogMessageSaxReader(Consumer<LogMessage> logMessageConsumer) {
		this.logMessageConsumer = logMessageConsumer;
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {

		switch (qName) {

		case Tags.LOG_MESSAGE:
			this.id = attributes.getValue(Tags.ID);
			this.dateTime = ISO8601.parseToZdt(attributes.getValue(Tags.DATE));
			this.realm = attributes.getValue(Tags.REALM);
			break;

		case Tags.USERNAME:
		case Tags.LOCATOR:
		case Tags.SEVERITY:
		case Tags.KEY:
		case Tags.MESSAGE:
		case Tags.EXCEPTION:
		case Tags.STATE:

			this.sb = new StringBuilder();
			break;

		case Tags.PROPERTIES:
			this.properties = new Properties();
			break;

		case Tags.PROPERTY:
			String key = attributes.getValue(Tags.KEY);
			String value = attributes.getValue(Tags.VALUE);
			this.properties.setProperty(key, value);
			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) throws SAXException {

		switch (qName) {

		case Tags.LOG_MESSAGE:
			if (this.state == null)
				this.state = LogMessageState.Information;

			LogMessage logMessage = new LogMessage(this.id, this.dateTime, this.realm, this.username, this.locator,
					this.severity, this.state, this.bundle, this.key, this.properties, this.message, this.exception);
			this.logMessageConsumer.accept(logMessage);
			break;

		case Tags.USERNAME:
			this.username = this.sb.toString();
			this.sb = null;
			break;

		case Tags.LOCATOR:
			this.locator = Locator.valueOf(this.sb.toString());
			this.sb = null;
			break;

		case Tags.SEVERITY:
			this.severity = LogSeverity.valueOf(this.sb.toString());
			this.sb = null;
			break;

		case Tags.STATE:
			this.state = LogMessageState.valueOf(this.sb.toString());
			this.sb = null;
			break;

		case Tags.BUNDLE:
			this.bundle = this.sb.toString();
			this.sb = null;
			break;

		case Tags.KEY:
			this.key = this.sb.toString();
			this.sb = null;
			break;

		case Tags.MESSAGE:
			this.message = this.sb.toString();
			this.sb = null;
			break;

		case Tags.EXCEPTION:
			this.exception = this.sb.toString();
			this.sb = null;
			break;

		case Tags.PROPERTIES:
		case Tags.PROPERTY:
			break;

		default:
			throw new IllegalArgumentException(
					MessageFormat.format("The element ''{0}'' is unhandled!", qName)); //$NON-NLS-1$
		}
	}

	@Override
	public void characters(char[] ch, int start, int length) throws SAXException {
		if (this.sb != null)
			this.sb.append(ch, start, length);
	}
}
