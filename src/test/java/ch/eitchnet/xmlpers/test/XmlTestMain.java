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
package ch.eitchnet.xmlpers.test;

import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.utils.helper.XmlHelper;
import ch.eitchnet.xmlpers.test.model.ModelBuilder;
import ch.eitchnet.xmlpers.test.model.Parameter;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class XmlTestMain {

	private static final Logger logger = LoggerFactory.getLogger(XmlTestMain.class);

	private static Resource res;

	public static void main(String[] args) throws Exception {

		res = ModelBuilder.createResource();

		logger.info("Writing Res:\n" + res);

		writeSax(res);
		writeDom(res);

		List<Resource> resoures;
		resoures = readDom();
		logger.info("Parsed Resources:\n" + resoures);
		resoures = readSax();
		logger.info("Parsed Resources:\n" + resoures);
	}

	/**
	 * @return
	 * 
	 */
	private static List<Resource> readDom() throws Exception {

		File file = new File("target/res_dom.xml");
		DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		Document document = docBuilder.parse(file);
		Element rootElement = document.getDocumentElement();

		List<Resource> resources = new ArrayList<Resource>();

		NodeList resElements = rootElement.getElementsByTagName("Resource");
		for (int i = 0; i < resElements.getLength(); i++) {
			Element resElement = (Element) resElements.item(i);
			String id = resElement.getAttribute("id");
			String name = resElement.getAttribute("name");
			String type = resElement.getAttribute("type");

			Resource res = new Resource();
			res.setId(id);
			res.setName(name);
			res.setType(type);

			NodeList paramElements = resElement.getElementsByTagName("Parameter");
			parseParameters(res, paramElements);

			resources.add(res);
		}

		logger.info("DOM parsed file " + file.getAbsolutePath());

		return resources;
	}

	private static void parseParameters(Resource res, NodeList paramElements) {
		for (int i = 0; i < paramElements.getLength(); i++) {
			Element paramElement = (Element) paramElements.item(i);
			String id = paramElement.getAttribute("id");
			String name = paramElement.getAttribute("name");
			String type = paramElement.getAttribute("type");
			String value = paramElement.getAttribute("value");

			Parameter param = new Parameter();
			param.setId(id);
			param.setName(name);
			param.setType(type);
			param.setValue(value);

			res.addParameter(param);
		}
	}

	/**
	 * @return
	 * 
	 */
	private static List<Resource> readSax() throws Exception {

		final List<Resource> resources = new ArrayList<>();
		final Resource[] currentRes = new Resource[1];

		DefaultHandler xmlHandler = new DefaultHandler() {

			@Override
			public void startElement(String uri, String localName, String qName, Attributes attributes)
					throws SAXException {

				switch (qName) {
				case "Resource":
					Resource res = new Resource();
					res.setId(attributes.getValue("id"));
					res.setName(attributes.getValue("name"));
					res.setType(attributes.getValue("type"));
					currentRes[0] = res;
					resources.add(res);
					break;
				case "Parameter":
					Parameter param = new Parameter();
					param.setId(attributes.getValue("id"));
					param.setName(attributes.getValue("name"));
					param.setType(attributes.getValue("type"));
					param.setValue(attributes.getValue("value"));
					currentRes[0].addParameter(param);
					break;
				case "model":
					break;
				default:
					throw new IllegalArgumentException("The element '" + qName + "' is unhandled!");
				}
			}
		};

		SAXParserFactory spf = SAXParserFactory.newInstance();
		SAXParser sp = spf.newSAXParser();
		File file = new File("target/res_sax.xml");
		sp.parse(file, xmlHandler);

		logger.info("SAX parsed file " + file.getAbsolutePath());

		return resources;
	}

	private static void writeDom(Resource res) throws Exception {

		DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		Document doc = docBuilder.newDocument();

		Element resElement = doc.createElement("Resource");
		resElement.setAttribute("id", res.getId());
		resElement.setAttribute("name", res.getName());
		resElement.setAttribute("type", res.getType());

		for (String paramId : res.getParameterKeySet()) {
			Parameter param = res.getParameterBy(paramId);
			Element paramElement = doc.createElement("Parameter");
			paramElement.setAttribute("id", param.getId());
			paramElement.setAttribute("name", param.getName());
			paramElement.setAttribute("type", param.getType());
			paramElement.setAttribute("value", param.getValue());
			resElement.appendChild(paramElement);
		}

		Element rootElement = doc.createElement("model");
		rootElement.appendChild(resElement);

		doc.appendChild(rootElement);

		String lineSep = System.getProperty(XmlHelper.PROP_LINE_SEPARATOR);
		try {

			String encoding = doc.getInputEncoding();
			if (encoding == null || encoding.isEmpty()) {
				logger.info("No encoding passed. Using default encoding " + XmlHelper.DEFAULT_ENCODING);
				encoding = XmlHelper.DEFAULT_ENCODING;
			}

			if (!lineSep.equals("\n")) {
				logger.info("Overriding line separator to \\n");
				System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, "\n");
			}

			// Set up a transformer
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer transformer = transfac.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			transformer.setOutputProperty(OutputKeys.METHOD, "xml");
			transformer.setOutputProperty(OutputKeys.ENCODING, encoding);
			transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "2");
			// transformer.setOutputProperty("{http://xml.apache.org/xalan}line-separator", "\t");

			// Transform to file
			File file = new File("target/res_dom.xml");
			StreamResult result = new StreamResult(file);
			Source xmlSource = new DOMSource(doc);
			transformer.transform(xmlSource, result);

			logger.info("Wrote DOM to " + file.getAbsolutePath());

		} catch (Exception e) {

			throw new XmlException("Exception while exporting to file: " + e, e);

		} finally {

			System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, lineSep);
		}
	}

	private static void writeSax(Resource res) throws Exception {
		XMLOutputFactory factory = XMLOutputFactory.newInstance();

		File file = new File("target/res_sax.xml");
		try (FileWriter fileWriter = new FileWriter(file)) {
			XMLStreamWriter writer = factory.createXMLStreamWriter(fileWriter);

			writer = new IndentingXMLStreamWriter(writer);

			writer.writeStartDocument("utf-8", "1.0");
			writer.writeStartElement("model");

			writer.writeStartElement("Resource");
			writer.writeAttribute("id", res.getId());
			writer.writeAttribute("name", res.getName());
			writer.writeAttribute("type", res.getType());

			for (String paramId : res.getParameterKeySet()) {
				Parameter param = res.getParameterBy(paramId);
				writer.writeEmptyElement("Parameter");
				writer.writeAttribute("id", param.getId());
				writer.writeAttribute("name", param.getName());
				writer.writeAttribute("type", param.getType());
				writer.writeAttribute("value", param.getValue());
			}

			//writer.writeEmptyElement("data");
			//writer.writeAttribute("name", "value");
			////writer.writeEndElement();
			//writer.writeEmptyElement("stuff");
			//writer.writeAttribute("attr", "attrVal");

			writer.writeEndElement();
			writer.writeEndDocument();

			writer.flush();
			writer.close();
			logger.info("Wrote SAX to " + file.getAbsolutePath());

			//Transformer transformer = TransformerFactory.newInstance().newTransformer();
			//Result outputTarget = new StaxR;
			//Source xmlSource;
			//transformer.transform(xmlSource, outputTarget);
		}
	}
}
