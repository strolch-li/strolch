package li.strolch.persistence.xml.model;

import static li.strolch.utils.helper.StringHelper.isEmpty;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.text.MessageFormat;
import java.util.Properties;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.log.LogMessage;
import li.strolch.model.Tags;
import li.strolch.utils.iso8601.ISO8601;

public class LogMessageToSaxWriterVisitor {

	protected XMLStreamWriter writer;

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
