package li.strolch.model.xml;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import java.io.StringWriter;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.activity.Activity;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.dbc.DBC;

public class StrolchElementToXmlStringVisitor implements StrolchRootElementVisitor<String> {

	private boolean withDocument;

	public StrolchElementToXmlStringVisitor() {
		this.withDocument = true;
	}

	public StrolchElementToXmlStringVisitor withoutDocument() {
		this.withDocument = false;
		return this;
	}

	private String visit(StrolchRootElement element) {

		try {
			StringWriter stringWriter = new StringWriter();
			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			XMLStreamWriter writer = factory.createXMLStreamWriter(stringWriter);
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			if (this.withDocument)
				writer.writeStartDocument(StrolchModelConstants.DEFAULT_ENCODING,
						StrolchModelConstants.DEFAULT_XML_VERSION);

			element.accept(new StrolchElementToSaxWriterVisitor(writer));

			if (this.withDocument)
				writer.writeEndDocument();

			return stringWriter.toString();

		} catch (Exception e) {
			throw new RuntimeException(
					"Failed to format Element " + element.getLocator() + " to xml string due to " + e.getMessage(), e);
		}
	}

	@Override
	public String visitOrder(Order element) {
		DBC.PRE.assertNotNull("Order my not be null!", element);
		return visit(element);
	}

	@Override
	public String visitResource(Resource element) {
		DBC.PRE.assertNotNull("Resource my not be null!", element);
		return visit(element);
	}

	@Override
	public String visitActivity(Activity element) {
		DBC.PRE.assertNotNull("Activity my not be null!", element);
		return visit(element);
	}
}
