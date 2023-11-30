package li.strolch.model.xml;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.*;
import java.nio.file.Files;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static li.strolch.model.StrolchModelConstants.DEFAULT_ENCODING;
import static li.strolch.model.StrolchModelConstants.DEFAULT_XML_VERSION;
import static li.strolch.utils.helper.XmlHelper.getSaxParser;

public class StrolchXmlHelper {

	private static final Logger logger = LoggerFactory.getLogger(StrolchXmlHelper.class);
	public static final String W3C_XSD_URI = "http://www.w3.org/2001/XMLSchema-instance";
	public static final String STROLCH_MODEL_NS_URI = "https://strolch.li/xsd/StrolchModel-2.0.xsd";
	public static final String STROLCH_MODEL_NS = "https://strolch.li/schema/StrolchModel.xsd";
	public static final String STROLCH_MODEL_XSD_LOCATION = STROLCH_MODEL_NS + " " + STROLCH_MODEL_NS_URI;

	public static Resource parseAndReturnResource(String xml, String id) {
		return parse(xml).getResource(id);
	}

	public static Resource parseAndReturnResource(File file, String id) {
		return parse(file).getResource(id);
	}

	public static Order parseAndReturnOrder(File file, String id) {
		return parse(file).getOrder(id);
	}

	public static Activity parseAndReturnActivity(File file, String id) {
		return parse(file).getActivity(id);
	}

	public static Map<String, StrolchRootElement> parseToMap(File file) {
		return parseFile(file).stream().collect(Collectors.toMap(StrolchRootElement::getId, e -> e));
	}

	public static Map<String, StrolchRootElement> parseToMap(InputStream stream, String encoding) {
		return parseStream(stream, encoding).stream().collect(Collectors.toMap(StrolchRootElement::getId, e -> e));
	}

	public static SimpleStrolchElementListener parse(File file) {
		SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener;
	}

	private static SimpleStrolchElementListener parse(String xml) {
		try {
			SimpleStrolchElementListener elementListener = new SimpleStrolchElementListener();
			getSaxParser().parse(new ByteArrayInputStream(xml.getBytes(UTF_8)), new XmlModelSaxReader(elementListener));
			return elementListener;
		} catch (Exception e) {
			throw new IllegalStateException("Failed to parse XML", e);
		}
	}

	public static List<StrolchRootElement> parseFile(File file) {
		StrolchElementListenerToListListener elementListener = new StrolchElementListenerToListListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.getElements();
	}

	public static Stream<StrolchRootElement> parseFileAsStream(File file) {
		StrolchElementListenerToListListener elementListener = new StrolchElementListenerToListListener();
		new XmlModelSaxFileReader(elementListener, file, false).parseFile();
		return elementListener.streamElements();
	}

	public static List<StrolchRootElement> parseStream(InputStream stream, String encoding) {
		StrolchElementListenerToListListener elementListener = new StrolchElementListenerToListListener();
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

	public static void writeToFile(File file, Stream<? extends StrolchRootElement> elements) {
		writeToFile(file, elements, false);
	}

	public static void writeToFile(File file, Stream<? extends StrolchRootElement> elements, boolean withNamespace) {
		int size;
		try (OutputStream out = Files.newOutputStream(file.toPath())) {
			size = writeToStream(out, elements, withNamespace);
		} catch (Exception e) {
			throw new RuntimeException("Failed to write elements to " + file, e);
		}

		logger.info("Wrote " + size + " elements to file " + file);
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
		elements.forEach(element -> element.accept(new StrolchElementToSaxWriterVisitor(writer)));
		writer.writeEndDocument();
	}

	public static int writeToStream(OutputStream out, Stream<? extends StrolchRootElement> elements) throws Exception {
		return writeToStream(out, elements, false);
	}

	public static int writeToStream(OutputStream out, Stream<? extends StrolchRootElement> elements,
			boolean withNamespace) throws Exception {
		XMLStreamWriter writer = prepareXmlStreamWriter(out, withNamespace);
		AtomicInteger size = new AtomicInteger();
		elements.peek(e -> size.incrementAndGet()).forEach(e -> e.accept(new StrolchElementToSaxWriterVisitor(writer)));
		writer.writeEndDocument();
		return size.get();
	}

	public static XMLStreamWriter prepareXmlStreamWriter(Writer writer)
			throws FactoryConfigurationError, XMLStreamException {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter xmlStreamWriter = factory.createXMLStreamWriter(writer);

		return prepareXmlStreamWriter(xmlStreamWriter);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(OutputStream out)
			throws FactoryConfigurationError, XMLStreamException {
		return prepareXmlStreamWriter(out, false);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(OutputStream out, boolean withNamespace)
			throws FactoryConfigurationError, XMLStreamException {

		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		XMLStreamWriter writer = factory.createXMLStreamWriter(out, DEFAULT_ENCODING);

		if (withNamespace)
			writer.setDefaultNamespace(STROLCH_MODEL_NS);

		return prepareXmlStreamWriter(writer, withNamespace);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(XMLStreamWriter writer)
			throws FactoryConfigurationError, XMLStreamException {
		return prepareXmlStreamWriter(writer, false);
	}

	public static XMLStreamWriter prepareXmlStreamWriter(XMLStreamWriter writer, boolean withNamespace)
			throws FactoryConfigurationError, XMLStreamException {

		writer = new IndentingXMLStreamWriter(writer);

		writer.writeStartDocument(DEFAULT_ENCODING, DEFAULT_XML_VERSION);
		writer.writeStartElement(Tags.STROLCH_MODEL);

		if (withNamespace) {
			writer.writeAttribute(STROLCH_MODEL_NS, "xmlns:xsi", W3C_XSD_URI);
			writer.writeAttribute(STROLCH_MODEL_NS, "xmlns", STROLCH_MODEL_NS);
			writer.writeAttribute(STROLCH_MODEL_NS, "xsi:schemaLocation", STROLCH_MODEL_XSD_LOCATION);
		}

		return writer;
	}
}
