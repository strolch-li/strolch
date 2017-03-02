package li.strolch.model.xml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.List;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;

public class StrolchXmlHelper {

	public static Resource parseAndReturnResource(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getResource(id);
	}

	public static Order parseAndReturnOrder(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getOrder(id);
	}

	public static Activity parseAndReturnActivity(File file, String id) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getActivity(id);
	}

	public static void writeToFile(File file, List<StrolchRootElement> elements) {

		try (FileOutputStream out = new FileOutputStream(file)) {

			XMLStreamWriter writer = openXmlStreamWriter(out);

			for (StrolchRootElement element : elements) {
				element.accept(new StrolchElementToSaxWriterVisitor(writer));
			}

		} catch (Exception e) {
			throw new RuntimeException("Failed to write elements to " + file, e);
		}
	}

	public static XMLStreamWriter openXmlStreamWriter(OutputStream out)
			throws FactoryConfigurationError, XMLStreamException {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, StrolchModelConstants.DEFAULT_ENCODING);
		writer = new IndentingXMLStreamWriter(writer);

		// start document
		writer.writeStartDocument(StrolchModelConstants.DEFAULT_ENCODING, StrolchModelConstants.DEFAULT_XML_VERSION);
		writer.writeStartElement(Tags.STROLCH_MODEL);
		return writer;
	}
}
