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
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.helper.XmlHelper;
import ch.eitchnet.xmlpers.api.DomParser;
import ch.eitchnet.xmlpers.api.DomUtil;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.SaxParser;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.impl.XmlPersistenceStreamWriter;

public class FileIo {

	public static final String DEFAULT_XML_VERSION = "1.0"; //$NON-NLS-1$
	public static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

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
			logger.info("Using default XML IO Handler SAX"); //$NON-NLS-1$
			writeSax(context);
			break;
		default:
			String msg = "The Xml IO Mode {0} is not supported!"; //$NON-NLS-1$
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
			logger.info("Using default XML IO Handler SAX"); //$NON-NLS-1$
			readSax(context);
			break;
		default:
			String msg = "The Xml IO Mode {0} is not supported!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, context.getIoMode());
			throw new UnsupportedOperationException(msg);
		}
	}

	private <T> void writeSax(PersistenceContext<T> context) {

		XMLStreamWriter writer = null;
		try {
			try (FileWriter fileWriter = new FileWriter(this.path);) {

				XMLOutputFactory factory = XMLOutputFactory.newInstance();
				writer = factory.createXMLStreamWriter(fileWriter);
				writer = new IndentingXMLStreamWriter(writer);

				// start document
				writer.writeStartDocument(DEFAULT_ENCODING, DEFAULT_XML_VERSION);

				// then delegate object writing to caller
				XmlPersistenceStreamWriter xmlWriter = new XmlPersistenceStreamWriter(writer);
				SaxParser<T> saxParser = context.getParserFactor().getSaxParser();
				saxParser.setObject(context.getObject());
				saxParser.write(xmlWriter);

				// and now end
				writer.writeEndDocument();
				writer.flush();
			}

		} catch (FactoryConfigurationError | XMLStreamException | IOException e) {
			if (this.path.exists())
				this.path.delete();
			String msg = "Writing to file failed due to internal error: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, e.getMessage());
			throw new XmlException(msg, e);
		}

		String msg = "Wrote SAX to {0}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.path.getAbsolutePath()));
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

			String msg = "Parsing failed due to internal error: {0}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, e.getMessage()), e);
		}

		String msg = "SAX parsed file {0}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.path.getAbsolutePath()));
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

			if (!lineSep.equals(StringHelper.NEW_LINE)) {
				logger.info("Overriding line separator to \\n"); //$NON-NLS-1$
				System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, StringHelper.NEW_LINE);
			}

			// Set up a transformer
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer transformer = transfac.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.ENCODING, encoding);
			transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "2"); //$NON-NLS-1$ //$NON-NLS-2$
			// transformer.setOutputProperty("{http://xml.apache.org/xalan}line-separator", "\t");

			// Transform to file
			StreamResult result = new StreamResult(this.path);
			Source xmlSource = new DOMSource(document);
			transformer.transform(xmlSource, result);

			String msg = MessageFormat.format("Wrote DOM to {0}", this.path.getAbsolutePath()); //$NON-NLS-1$
			logger.info(msg);

		} catch (TransformerFactoryConfigurationError | TransformerException e) {
			if (this.path.exists())
				this.path.delete();
			String msg = "Writing to file failed due to internal error: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, e.getMessage());
			throw new XmlException(msg, e);
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
			String msg = "Parsing failed due to internal error: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, e.getMessage());
			throw new XmlPersistenceException(msg, e);
		}

		String msg = "DOM parsed file {0}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, this.path.getAbsolutePath());
		logger.info(msg);
	}
}