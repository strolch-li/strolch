/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.xmlpers.impl;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.utils.helper.XmlHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceContextData;
import ch.eitchnet.xmlpers.api.XmlPersistenceDomContextData;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.api.XmlPersistenceFileHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceDomHandler implements XmlPersistenceFileHandler {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceDomHandler.class);

	public DocumentBuilder createDocumentBuilder() throws ParserConfigurationException {
		DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		return docBuilder;
	}

	@Override
	public void read(XmlPersistenceContextData contextData) {

		XmlPersistenceDomContextData cd = (XmlPersistenceDomContextData) contextData;

		// check assertions
		if (cd.getFile() == null)
			throw new IllegalStateException("No file has been set on the context data!");

		try {
			DocumentBuilder docBuilder = createDocumentBuilder();
			File file = cd.getFile();
			Document document = docBuilder.parse(file);
			cd.setDocument(document);
		} catch (ParserConfigurationException | SAXException | IOException e) {
			throw new XmlPersistenceException("Parsing failed due to internal error: " + e.getMessage(), e);
		}

		logger.info("DOM parsed file " + cd.getFile().getAbsolutePath());
	}

	@Override
	public void write(XmlPersistenceContextData contextData) {

		XmlPersistenceDomContextData cd = (XmlPersistenceDomContextData) contextData;

		// check assertions
		if (cd.getFile() == null)
			throw new IllegalStateException("No file has been set on the context data!");
		if (cd.getDocument() == null)
			throw new IllegalStateException("No document has been set on the context data!");

		String lineSep = System.getProperty(XmlHelper.PROP_LINE_SEPARATOR);
		try {
			Document document = cd.getDocument();
			String encoding = document.getInputEncoding();
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
			Source xmlSource = new DOMSource(document);
			transformer.transform(xmlSource, result);

			logger.info("Wrote DOM to " + file.getAbsolutePath());

		} catch (TransformerFactoryConfigurationError | TransformerException e) {
			throw new XmlException("Writing to file failed due to internal error: " + e.getMessage(), e);
		} finally {
			System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, lineSep);
		}
	}
}
