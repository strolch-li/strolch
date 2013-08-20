/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of ch.eitchnet.java.utils.
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.helper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.exceptions.XmlException;

/**
 * Helper class for performing XML based tasks
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlHelper {

	/**
	 * PROP_LINE_SEPARATOR = "line.separator" : the system property to fetch defined line separator
	 */
	public static final String PROP_LINE_SEPARATOR = "line.separator";

	/**
	 * UNIX_LINE_SEP = "\n" : mostly we want this line separator, instead of the windows version
	 */
	public static final String UNIX_LINE_SEP = "\n";

	/**
	 * DEFAULT_ENCODING = "utf-8" : defines the default UTF-8 encoding expected of XML files
	 */
	public static final String DEFAULT_ENCODING = "utf-8";

	private static final Logger logger = LoggerFactory.getLogger(XmlHelper.class);

	/**
	 * Parses an XML file on the file system and returns the resulting {@link Document} object
	 * 
	 * @param xmlFile
	 *            the {@link File} which has the path to the XML file to read
	 */
	public static void parseDocument(File xmlFile, DefaultHandler xmlHandler) {

		try {
			XmlHelper.logger.info("Parsing XML document " + xmlFile.getAbsolutePath());
			parseDocument(new FileInputStream(xmlFile), xmlHandler);
		} catch (FileNotFoundException e) {
			throw new XmlException("The XML file could not be read: " + xmlFile.getAbsolutePath(), e);
		}
	}

	/**
	 * Parses an XML file on the file system and returns the resulting {@link Document} object
	 * 
	 * @param xmlFileInputStream
	 *            the XML {@link InputStream} which is to be parsed
	 */
	public static void parseDocument(InputStream xmlFileInputStream, DefaultHandler xmlHandler) {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();

			SAXParser sp = spf.newSAXParser();
			sp.parse(xmlFileInputStream, xmlHandler);

		} catch (ParserConfigurationException e) {
			throw new XmlException("Failed to initialize a SAX Parser: " + e.getMessage(), e);
		} catch (SAXException e) {
			throw new XmlException("The XML stream is not parseable: " + e.getMessage(), e);
		} catch (IOException e) {
			throw new XmlException("The XML stream not be read: " + e.getMessage(), e);
		}
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 * 
	 * @param document
	 *            the {@link Document} to write to the file system
	 * @param file
	 *            the {@link File} describing the path on the file system where the XML file should be written to
	 * 
	 * @throws RuntimeException
	 *             if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeDocument(Document document, File file) throws RuntimeException {

		XmlHelper.logger.info("Exporting document element " + document.getNodeName() + " to " + file.getAbsolutePath());

		String lineSep = System.getProperty(PROP_LINE_SEPARATOR);
		try {

			String encoding = document.getInputEncoding();
			if (encoding == null || encoding.isEmpty()) {
				XmlHelper.logger.info("No encoding passed. Using default encoding " + XmlHelper.DEFAULT_ENCODING);
				encoding = XmlHelper.DEFAULT_ENCODING;
			}

			if (!lineSep.equals("\n")) {
				XmlHelper.logger.info("Overriding line separator to \\n");
				System.setProperty(PROP_LINE_SEPARATOR, UNIX_LINE_SEP);
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
			StreamResult result = new StreamResult(file);
			Source xmlSource = new DOMSource(document);
			transformer.transform(xmlSource, result);

		} catch (Exception e) {

			throw new XmlException("Exception while exporting to file: " + e, e);

		} finally {

			System.setProperty(PROP_LINE_SEPARATOR, lineSep);
		}
	}

	/**
	 * Writes an {@link Element} to an XML file on the file system
	 * 
	 * @param rootElement
	 *            the {@link Element} to write to the file system
	 * @param file
	 *            the {@link File} describing the path on the file system where the XML file should be written to
	 * @param encoding
	 *            encoding to use to write the file
	 * 
	 * @throws RuntimeException
	 *             if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeElement(Element rootElement, File file, String encoding) throws RuntimeException {

		Document document = createDocument();
		document.appendChild(rootElement);
		XmlHelper.writeDocument(document, file);
	}

	/**
	 * Returns a new document instance
	 * 
	 * @return a new document instance
	 * 
	 * @throws RuntimeException
	 *             if something went wrong while creating the XML configuration
	 */
	public static Document createDocument() throws RuntimeException {
		try {

			DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
			Document document = docBuilder.newDocument();

			return document;

		} catch (DOMException e) {
			throw new XmlException("Failed to create Document: " + e.getLocalizedMessage(), e);
		} catch (ParserConfigurationException e) {
			throw new XmlException("Failed to create Document: " + e.getLocalizedMessage(), e);
		}
	}
}
