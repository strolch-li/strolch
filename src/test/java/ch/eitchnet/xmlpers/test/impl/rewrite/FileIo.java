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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.MessageFormat;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
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
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.utils.helper.XmlHelper;
import ch.eitchnet.xmlpers.api.DomUtil;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.impl.XmlPersistenceStreamWriter;

public class FileIo {

	private static final Logger logger = LoggerFactory.getLogger(FileIo.class);

	private final File path;

	public FileIo(File path) {
		this.path = path;
	}

	public <T> void write(PersistenceContext<T> context) {
		switch (context.getIoMode()) {
		case DOM:
			writeDom(context);
			break;
		case SAX:
			writeSax(context);
			break;
		case DEFAULT:
			logger.info("Using default XML IO Handler SAX");
			writeSax(context);
			break;
		default:
			String msg = "The Xml IO Mode {0} is not supported!";
			msg = MessageFormat.format(msg, context.getIoMode());
			throw new UnsupportedOperationException(msg);
		}
	}

	public <T> void read(PersistenceContext<T> context) {
		switch (context.getIoMode()) {
		case DOM:
			readDom(context);
			break;
		case SAX:
			readSax(context);
			break;
		case DEFAULT:
			logger.info("Using default XML IO Handler SAX");
			readSax(context);
			break;
		default:
			String msg = "The Xml IO Mode {0} is not supported!";
			msg = MessageFormat.format(msg, context.getIoMode());
			throw new UnsupportedOperationException(msg);
		}
	}

	private <T> void writeSax(PersistenceContext<T> context) {

		XMLStreamWriter writer = null;
		try {
			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			writer = factory.createXMLStreamWriter(new FileWriter(this.path));
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			writer.writeStartDocument("utf-8", "1.0");

			// then delegate object writing to caller
			XmlPersistenceStreamWriter xmlWriter = new XmlPersistenceStreamWriter(writer);
			SaxParser<T> saxParser = context.getParserFactor().getSaxParser();
			saxParser.setObject(context.getObject());
			saxParser.write(xmlWriter);

			// and now end
			writer.writeEndDocument();
			writer.flush();

		} catch (FactoryConfigurationError | XMLStreamException | IOException e) {
			if (this.path.exists())
				this.path.delete();
			throw new XmlException("Writing to file failed due to internal error: " + e.getMessage(), e);
		} finally {
			if (writer != null) {
				try {
					writer.close();
				} catch (Exception e) {
					logger.error("Failed to close stream: " + e.getMessage());
				}
			}
		}

		logger.info("Wrote SAX to " + this.path.getAbsolutePath());
	}

	private <T> void readSax(PersistenceContext<T> context) {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			SaxParser<T> saxParser = context.getParserFactor().getSaxParser();
			DefaultHandler defaultHandler = saxParser.getDefaultHandler();
			sp.parse(this.path, defaultHandler);
			context.setObject(saxParser.getObject());

		} catch (ParserConfigurationException | SAXException | IOException e) {

			throw new XmlPersistenceException("Parsing failed due to internal error: " + e.getMessage(), e);
		}

		logger.info("SAX parsed file " + this.path.getAbsolutePath());
	}

	private <T> void writeDom(PersistenceContext<T> context) {
		String lineSep = System.getProperty(XmlHelper.PROP_LINE_SEPARATOR);
		try {
			DomParser<T> domParser = context.getParserFactor().getDomParser();
			domParser.setObject(context.getObject());
			Document document = domParser.toDom();
			String encoding = document.getInputEncoding();
			if (encoding == null || encoding.isEmpty()) {
				// logger.info("No encoding passed. Using default encoding " + XmlHelper.DEFAULT_ENCODING);
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
			StreamResult result = new StreamResult(this.path);
			Source xmlSource = new DOMSource(document);
			transformer.transform(xmlSource, result);

			logger.info("Wrote DOM to " + this.path.getAbsolutePath());

		} catch (TransformerFactoryConfigurationError | TransformerException e) {
			if (this.path.exists())
				this.path.delete();
			throw new XmlException("Writing to file failed due to internal error: " + e.getMessage(), e);
		} finally {
			System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, lineSep);
		}
	}

	private <T> void readDom(PersistenceContext<T> context) {
		try {
			DocumentBuilder docBuilder = DomUtil.createDocumentBuilder();
			Document document = docBuilder.parse(this.path);
			DomParser<T> domParser = context.getParserFactor().getDomParser();
			domParser.fromDom(document);
			context.setObject(domParser.getObject());
		} catch (SAXException | IOException e) {
			throw new XmlPersistenceException("Parsing failed due to internal error: " + e.getMessage(), e);
		}

		logger.info("DOM parsed file " + this.path.getAbsolutePath());
	}
}