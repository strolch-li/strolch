package li.strolch.model.xml;

import static li.strolch.model.StrolchModelConstants.DEFAULT_ENCODING;
import static li.strolch.model.StrolchModelConstants.DEFAULT_XML_VERSION;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.file.Files;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
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

	public static Map<String, StrolchRootElement> parseToMap(File file) {
		return parseFile(file).stream().collect(Collectors.toMap(StrolchRootElement::getId, e -> e));
	}

	public static Map<String, StrolchRootElement> parseToMap(InputStream stream, String encoding) {
		return parseStream(stream, encoding).stream().collect(Collectors.toMap(StrolchRootElement::getId, e -> e));
	}

	public static List<StrolchRootElement> parseFile(File file) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getElements();
	}

	public static List<StrolchRootElement> parseStream(InputStream stream, String encoding) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxStreamReader(elementListener, stream, encoding).parseStream();
		return elementListener.getElements();
	}

	public static void writeToFile(File file, Collection<? extends StrolchRootElement> elements) {
		try (OutputStream out = Files.newOutputStream(file.toPath())) {
			writeToStream(out, elements);
		} catch (Exception e) {
			throw new RuntimeException("Failed to write elements to " + file, e);
		}

		logger.info("Wrote " + elements.size() + " elements to file " + file);
	}

	public static void writeToWriter(Writer writer, Collection<? extends StrolchRootElement> elements) {
		try {
			XMLStreamWriter xmlStreamWriter = prepareXmlStreamWriter(writer);

			for (StrolchRootElement element : elements) {
				element.accept(new StrolchElementToSaxWriterVisitor(xmlStreamWriter));
			}

			xmlStreamWriter.writeEndDocument();

		} catch (Exception e) {
			throw new RuntimeException("Failed to write elements to " + writer, e);
		}
	}

	public static void writeToStream(OutputStream out, Collection<? extends StrolchRootElement> elements)
			throws Exception {

		XMLStreamWriter writer = prepareXmlStreamWriter(out);

		for (StrolchRootElement element : elements) {
			element.accept(new StrolchElementToSaxWriterVisitor(writer));
		}

		writer.writeEndDocument();
	}

	public static XMLStreamWriter prepareXmlStreamWriter(Writer writer)
			throws FactoryConfigurationError, XMLStreamException {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter xmlStreamWriter = factory.createXMLStreamWriter(writer);

		return prepareXmlStreamWriter(xmlStreamWriter);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(OutputStream out)
			throws FactoryConfigurationError, XMLStreamException {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, DEFAULT_ENCODING);

		return prepareXmlStreamWriter(writer);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(XMLStreamWriter writer)
			throws FactoryConfigurationError, XMLStreamException {

		writer = new IndentingXMLStreamWriter(writer);

		writer.writeStartDocument(DEFAULT_ENCODING, DEFAULT_XML_VERSION);
		writer.writeStartElement(Tags.STROLCH_MODEL);
		return writer;
	}
}
