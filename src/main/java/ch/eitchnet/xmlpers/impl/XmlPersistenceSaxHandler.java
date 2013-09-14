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
import java.io.FileWriter;
import java.io.IOException;

import javanet.staxutils.IndentingXMLStreamWriter;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.xmlpers.api.XmlPersistenceContextData;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.api.XmlPersistenceFileHandler;
import ch.eitchnet.xmlpers.api.XmlPersistenceSaxContextData;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceSaxHandler implements XmlPersistenceFileHandler {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceSaxHandler.class);

	@Override
	public void read(XmlPersistenceContextData contextData) {

		XmlPersistenceSaxContextData cd = (XmlPersistenceSaxContextData) contextData;

		// check assertions
		if (cd.getFile() == null)
			throw new IllegalStateException("No file has been set on the context data!");
		if (cd.getDefaultHandler() == null)
			throw new IllegalStateException("No DefaultHandler has been set on the context data!");

		File file;
		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			file = cd.getFile();
			DefaultHandler defaultHandler = cd.getDefaultHandler();
			sp.parse(file, defaultHandler);

		} catch (ParserConfigurationException | SAXException | IOException e) {

			throw new XmlPersistenceException("Parsing failed due to internal error: " + e.getMessage(), e);
		}

		logger.info("SAX parsed file " + file.getAbsolutePath());
	}

	@Override
	public void write(XmlPersistenceContextData contextData) {

		XmlPersistenceSaxContextData cd = (XmlPersistenceSaxContextData) contextData;

		// check assertions
		if (cd.getFile() == null)
			throw new IllegalStateException("No file has been set on the context data!");
		if (cd.getXmlWriter() == null)
			throw new IllegalStateException("No Xml writer has been set on the context data!");

		XMLStreamWriter writer = null;
		try {
			XMLOutputFactory factory = XMLOutputFactory.newInstance();
			File file = cd.getFile();
			writer = factory.createXMLStreamWriter(new FileWriter(file));
			writer = new IndentingXMLStreamWriter(writer);

			// start document
			writer.writeStartDocument("utf-8", "1.0");

			// then delegate object writing to caller
			XmlPersistenceStreamWriter xmlWriter = new XmlPersistenceStreamWriter(writer);
			cd.getXmlWriter().write(xmlWriter);

			// and now end
			writer.writeEndDocument();
			writer.flush();

		} catch (FactoryConfigurationError | XMLStreamException | IOException e) {
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

		logger.info("Wrote SAX to " + cd.getFile().getAbsolutePath());
	}
}
