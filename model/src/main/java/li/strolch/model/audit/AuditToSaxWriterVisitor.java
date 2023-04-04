package li.strolch.model.audit;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.text.MessageFormat;

import li.strolch.exception.StrolchException;
import li.strolch.model.Tags;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class AuditToSaxWriterVisitor implements AuditVisitor<Void> {

	protected XMLStreamWriter writer;

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
		this.writer.writeAttribute(Tags.Audit.ID, audit.getId().toString());

		writeElem(Tags.Audit.USERNAME, audit.getUsername());

		writeElem(Tags.Audit.FIRSTNAME, audit.getFirstname());
		writeElem(Tags.Audit.LASTNAME, audit.getLastname());
		writeElem(Tags.Audit.DATE, ISO8601FormatFactory.getInstance().formatDate(audit.getDate()));

		writeElem(Tags.Audit.ELEMENT_TYPE, audit.getElementType());
		writeElem(Tags.Audit.ELEMENT_SUB_TYPE, audit.getElementSubType());
		writeElem(Tags.Audit.ELEMENT_ACCESSED, audit.getElementAccessed());

		if (audit.getNewVersion() != null)
			writeElem(Tags.Audit.NEW_VERSION, ISO8601FormatFactory.getInstance().formatDate(audit.getNewVersion()));

		writeElem(Tags.Audit.ACTION, audit.getAction());
		writeElem(Tags.Audit.ACCESS_TYPE, audit.getAccessType().name());

		this.writer.writeEndElement();
	}

	private void writeElem(String tag, String text) throws XMLStreamException {
		if (StringHelper.isEmpty(text))
			return;

		this.writer.writeStartElement(tag);
		this.writer.writeCharacters(text);
		this.writer.writeEndElement();
	}
}
