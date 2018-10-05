package li.strolch.model.xml;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.Collection;
import java.util.List;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.*;
import li.strolch.model.activity.Activity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StrolchXmlHelper {

	private static final Logger logger = LoggerFactory.getLogger(StrolchXmlHelper.class);

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

	public static List<StrolchRootElement> parseFile(File file) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getElements();
	}

	public static void writeToFile(File file, Collection<StrolchRootElement> elements) {

		try (OutputStream out = Files.newOutputStream(file.toPath())) {

			XMLStreamWriter writer = openXmlStreamWriter(out);

			for (StrolchRootElement element : elements) {
				element.accept(new StrolchElementToSaxWriterVisitor(writer));
			}

			writer.writeEndDocument();

		} catch (Exception e) {
			throw new RuntimeException("Failed to write elements to " + file, e);
		}

		logger.info("Wrote " + elements.size() + " to file " + file);
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
