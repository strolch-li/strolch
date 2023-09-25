package li.strolch.privilege.helper;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static li.strolch.privilege.helper.XmlConstants.*;

public class XmlHelper {

	public static void writeStringMapElement(XMLStreamWriter xmlWriter, Map<String, String> parameterMap,
			String elementName, String valueElementName) throws XMLStreamException {
		writeStringMapElement(xmlWriter, parameterMap, elementName, valueElementName, ATTR_VALUE);
	}

	public static void writeStringMapElement(XMLStreamWriter xmlWriter, Map<String, String> parameterMap,
			String elementName, String valueElementName, String valueAttrName) throws XMLStreamException {
		if (parameterMap == null || parameterMap.isEmpty())
			return;

		xmlWriter.writeStartElement(elementName);

		List<String> propertyKeys = new ArrayList<>(parameterMap.keySet());
		propertyKeys.sort(null);
		for (String propertyKey : propertyKeys) {
			xmlWriter.writeEmptyElement(valueElementName);
			xmlWriter.writeAttribute(ATTR_NAME, propertyKey);
			xmlWriter.writeAttribute(valueAttrName, parameterMap.get(propertyKey));
		}

		xmlWriter.writeEndElement();
	}

	public static void writeStringList(IndentingXMLStreamWriter xmlWriter, String elementName, Set<String> values)
			throws XMLStreamException {
		List<String> denyList = new ArrayList<>(values);
		denyList.sort(null);
		for (String value : denyList) {
			writeStringElement(xmlWriter, elementName, value);
		}
	}

	public static void writeStringElement(IndentingXMLStreamWriter xmlWriter, String elementName, String value)
			throws XMLStreamException {
		xmlWriter.writeStartElement(elementName);
		xmlWriter.writeCharacters(value);
		xmlWriter.writeEndElement();
	}

	public static IndentingXMLStreamWriter openXmlStreamWriterDocument(Writer ioWriter) throws XMLStreamException {
		XMLOutputFactory factory = XMLOutputFactory.newInstance();
		IndentingXMLStreamWriter xmlWriter = new IndentingXMLStreamWriter(factory.createXMLStreamWriter(ioWriter));
		xmlWriter.setIndent("    ");

		// create document root
		xmlWriter.writeStartDocument(StandardCharsets.UTF_8.name(), "1.0");
		return xmlWriter;
	}
}
