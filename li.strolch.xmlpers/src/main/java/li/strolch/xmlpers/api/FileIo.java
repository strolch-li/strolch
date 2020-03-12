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
package li.strolch.xmlpers.api;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.text.MessageFormat;

import javanet.staxutils.IndentingXMLStreamWriter;
import li.strolch.utils.exceptions.XmlException;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.xmlpers.util.DomUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class FileIo {

	public static final String DEFAULT_XML_VERSION = "1.0"; //$NON-NLS-1$
	public static final String DEFAULT_ENCODING = "utf-8"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(FileIo.class);
	public static final String TMP_PREFIX = ".tmp_";

	private final File path;
	private final File tmpPath;

	public FileIo(File path) {
		this.path = path;
		this.tmpPath = new File(this.path.getParentFile(), TMP_PREFIX + this.path.getName());
	}

	public <T> void writeSax(PersistenceContext<T> ctx) {

		XMLStreamWriter xmlWriter;
		try {
			try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.tmpPath), DEFAULT_ENCODING)) {

				XMLOutputFactory factory = XMLOutputFactory.newInstance();
				xmlWriter = factory.createXMLStreamWriter(ioWriter);
				xmlWriter = new IndentingXMLStreamWriter(xmlWriter);

				// start document
				xmlWriter.writeStartDocument(DEFAULT_ENCODING, DEFAULT_XML_VERSION);

				// then delegate object writing to caller
				SaxParser<T> saxParser = ctx.getParserFactor().getSaxParser();
				saxParser.setObject(ctx.getObject());
				saxParser.write(xmlWriter);

				// and now end
				xmlWriter.writeEndDocument();
				xmlWriter.flush();
			}

			if (this.path.exists() && !this.path.delete())
				throw new IllegalStateException("Failed to delete existing file " + this.path.getAbsolutePath());
			if (!this.tmpPath.renameTo(this.path)) {
				throw new IllegalStateException(
						"Failed to rename temp file " + this.tmpPath.getName() + " to " + this.path.getAbsolutePath());
			}

		} catch (FactoryConfigurationError | XMLStreamException | IOException e) {
			if (this.tmpPath.exists()) {
				if (!this.tmpPath.delete())
					logger.error("Failed to delete existing temp file " + this.tmpPath.getAbsolutePath());
			}
			String msg = "Writing to file {0} failed due to internal error: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.path.getAbsolutePath(), e.getMessage());
			throw new XmlException(msg, e);
		}

		if (logger.isDebugEnabled()) {
			String msg = "Wrote SAX to {0}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.path.getAbsolutePath()));
		}
	}

	public <T> void readSax(PersistenceContext<T> ctx) {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();
			SAXParser sp = spf.newSAXParser();

			SaxParser<T> saxParser = ctx.getParserFactor().getSaxParser();
			DefaultHandler defaultHandler = saxParser.getDefaultHandler();
			sp.parse(this.path, defaultHandler);

			if (logger.isDebugEnabled()) {
				String msg = "SAX parsed file {0}"; //$NON-NLS-1$
				logger.info(MessageFormat.format(msg, this.path.getAbsolutePath()));
			}

			ctx.setObject(saxParser.getObject());

		} catch (ParserConfigurationException | SAXException | IOException e) {

			String msg = "Parsing of file {0} failed due to internal error: {1}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, this.path.getAbsolutePath(), e.getMessage()),
					e);
		}
	}

	public <T> void writeDom(PersistenceContext<T> ctx) {

		String lineSep = System.getProperty(XmlHelper.PROP_LINE_SEPARATOR);

		try {

			DomParser<T> domParser = ctx.getParserFactor().getDomParser();
			domParser.setObject(ctx.getObject());
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
			transformer
					.setOutputProperty("{http://xml.apache.org/xalan}indent-amount", "2"); //$NON-NLS-1$ //$NON-NLS-2$
			// transformer.setOutputProperty("{http://xml.apache.org/xalan}line-separator", "\t");

			// Transform to file
			try (Writer ioWriter = new OutputStreamWriter(new FileOutputStream(this.tmpPath), encoding)) {
				StreamResult result = new StreamResult(ioWriter);
				Source xmlSource = new DOMSource(document);
				transformer.transform(xmlSource, result);
			}

			if (logger.isDebugEnabled()) {
				String msg = MessageFormat.format("Wrote DOM to {0}", this.tmpPath.getAbsolutePath()); //$NON-NLS-1$
				logger.info(msg);
			}

			if (this.path.exists() && !this.path.delete())
				throw new IllegalStateException("Failed to delete existing file " + this.path.getAbsolutePath());
			if (!this.tmpPath.renameTo(this.path)) {
				throw new IllegalStateException(
						"Failed to rename temp file " + this.tmpPath.getName() + " to " + this.path.getAbsolutePath());
			}

		} catch (IOException | TransformerFactoryConfigurationError | TransformerException e) {
			if (this.tmpPath.exists()) {
				if (!this.tmpPath.delete())
					logger.error("Failed to delete existing temp file " + this.tmpPath.getAbsolutePath());
			}
			String msg = "Writing to file {0} failed due to internal error: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.tmpPath.getAbsolutePath(), e.getMessage());
			throw new XmlException(msg, e);

		} finally {
			System.setProperty(XmlHelper.PROP_LINE_SEPARATOR, lineSep);
		}
	}

	public <T> void readDom(PersistenceContext<T> ctx) {

		try {

			DocumentBuilder docBuilder = DomUtil.createDocumentBuilder();
			Document document = docBuilder.parse(this.path);
			DomParser<T> domParser = ctx.getParserFactor().getDomParser();
			domParser.fromDom(document);

			if (logger.isDebugEnabled()) {
				String msg = "DOM parsed file {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, this.path.getAbsolutePath());
				logger.info(msg);
			}

			ctx.setObject(domParser.getObject());

		} catch (SAXException | IOException e) {
			String msg = "Parsing {0} failed due to internal error: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, this.path.getAbsolutePath(), e.getMessage());
			throw new XmlPersistenceException(msg, e);
		}
	}
}