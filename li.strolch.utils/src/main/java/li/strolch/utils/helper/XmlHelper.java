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
package li.strolch.utils.helper;

import static li.strolch.utils.helper.FileHelper.getTempFile;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.parsers.*;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.nio.file.Files;
import java.text.MessageFormat;
import java.util.Set;

import li.strolch.utils.RemoveCRFilterWriter;
import li.strolch.utils.exceptions.XmlException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Helper class for performing XML based tasks
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class XmlHelper {

	/**
	 * PROP_LINE_SEPARATOR = "line.separator" : the system property to fetch defined line separator
	 */
	public static final String PROP_LINE_SEPARATOR = "line.separator"; //$NON-NLS-1$

	/**
	 * UNIX_LINE_SEP = "\n" : mostly we want this line separator, instead of the windows version
	 */
	public static final String UNIX_LINE_SEP = "\n"; //$NON-NLS-1$

	/**
	 * DEFAULT_ENCODING = "utf-8" : defines the default UTF-8 encoding expected of XML files
	 */
	public static final String DEFAULT_ENCODING = "UTF-8"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(XmlHelper.class);

	/**
	 * Parses an XML file on the file system and returns the resulting {@link Document} object
	 *
	 * @param xmlFile
	 * 		the {@link File} which has the path to the XML file to read
	 */
	public static void parseDocument(File xmlFile, DefaultHandler xmlHandler) {

		try (InputStream xmlFileInputStream = Files.newInputStream(xmlFile.toPath())) {

			parseDocument(xmlFileInputStream, xmlHandler);
			String msg = "SAX parsed file {0}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, xmlFile.getAbsolutePath()));

		} catch (IOException e) {
			String msg = "Failed to parse XML file: {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, xmlFile.getAbsolutePath(), e.getMessage());
			throw new XmlException(msg, e);
		}
	}

	/**
	 * Parses an XML file on the file system and returns the resulting {@link Document} object
	 *
	 * @param xmlFileInputStream
	 * 		the XML {@link InputStream} which is to be parsed
	 */
	public static void parseDocument(InputStream xmlFileInputStream, DefaultHandler xmlHandler) {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();

			SAXParser sp = spf.newSAXParser();
			sp.parse(xmlFileInputStream, xmlHandler);

		} catch (ParserConfigurationException e) {
			throw new XmlException("Failed to initialize a SAX Parser: " + e.getMessage(), e); //$NON-NLS-1$
		} catch (SAXException e) {
			throw new XmlException("The XML stream is not parseable: " + e.getMessage(), e); //$NON-NLS-1$
		} catch (IOException e) {
			throw new XmlException("The XML stream not be read: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * Parses an XML file on the file system and returns the resulting {@link Document} object
	 *
	 * @param xmlInputSource
	 * 		the XML {@link InputSource} which is to be parsed
	 */
	public static void parseDocument(InputSource xmlInputSource, DefaultHandler xmlHandler) {

		try {

			SAXParserFactory spf = SAXParserFactory.newInstance();

			SAXParser sp = spf.newSAXParser();
			sp.parse(xmlInputSource, xmlHandler);

		} catch (ParserConfigurationException e) {
			throw new XmlException("Failed to initialize a SAX Parser: " + e.getMessage(), e); //$NON-NLS-1$
		} catch (SAXException e) {
			throw new XmlException("The XML stream is not parseable: " + e.getMessage(), e); //$NON-NLS-1$
		} catch (IOException e) {
			throw new XmlException("The XML stream not be read: " + e.getMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * <p>Helper method to parse the given {@link File} using JAXB to the given object type</p>
	 *
	 * <p><b>Note:</b> The passed class must have the {@link XmlRootElement} annotation!</p>
	 *
	 * @param file
	 * 		the file to parse
	 * @param clazz
	 * 		the class for the returning object type
	 * @param <T>
	 * 		the type of object to return
	 *
	 * @return the parsed object
	 */
	public static <T> T parseAndUnmarshalFile(File file, Class<T> clazz) {
		try (FileInputStream fin = new FileInputStream(file)) {

			JAXBContext jaxbContext = JAXBContext.newInstance(clazz);
			Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();

			@SuppressWarnings("unchecked")
			T o = (T) unmarshaller.unmarshal(fin);

			return o;

		} catch (Exception e) {
			throw new IllegalStateException("Failed to parse file " + file.getAbsolutePath() + " to object " + clazz,
					e);
		}
	}

	/**
	 * Writes an {@link Element} to an XML file on the file system
	 *
	 * @param rootElement
	 * 		the {@link Element} to write to the file system
	 * @param file
	 * 		the {@link File} describing the path on the file system where the XML file should be written to
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeElement(Element rootElement, File file) throws RuntimeException {
		Document document = createDocument();
		document.appendChild(rootElement);
		XmlHelper.writeDocument(document, file, DEFAULT_ENCODING);
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 *
	 * @param document
	 * 		the {@link Document} to write to the file system
	 * @param file
	 * 		the {@link File} describing the path on the file system where the XML file should be written to
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeDocument(Document document, File file) throws RuntimeException {
		writeDocument(document, file, DEFAULT_ENCODING);
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 *
	 * @param document
	 * 		the {@link Document} to write to the file system
	 * @param file
	 * 		the {@link File} describing the path on the file system where the XML file should be written to
	 * @param encoding
	 * 		encoding to use to write the file
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeDocument(Document document, File file, String encoding) throws RuntimeException {
		try (FileOutputStream out = new FileOutputStream(file)) {
			writeDocument(document, new StreamResult(new RemoveCRFilterWriter(new OutputStreamWriter(out, encoding))),
					encoding);
		} catch (IOException e) {
			throw new XmlException("Failed to create Document: " + e.getLocalizedMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 *
	 * @param document
	 * 		the {@link Document} to write to the file system
	 * @param out
	 * 		stream to write document to
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeDocument(Document document, OutputStream out) throws RuntimeException {
		writeDocument(document, new StreamResult(new RemoveCRFilterWriter(new OutputStreamWriter(out))),
				DEFAULT_ENCODING);
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 *
	 * @param document
	 * 		the {@link Document} to write to the file system
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static String writeToString(Document document) throws RuntimeException {
		try {
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			writeDocument(document, new StreamResult(new RemoveCRFilterWriter(new OutputStreamWriter(out))),
					DEFAULT_ENCODING);
			return out.toString(DEFAULT_ENCODING);
		} catch (UnsupportedEncodingException e) {
			throw new XmlException("Failed to create Document: " + e.getLocalizedMessage(), e); //$NON-NLS-1$
		}
	}

	/**
	 * Writes a {@link Document} to an XML file on the file system
	 *
	 * @param document
	 * 		the {@link Document} to write to the file system
	 * @param streamResult
	 * 		the destination
	 * @param encoding
	 * 		encoding to use to write the file
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration, or writing the element
	 */
	public static void writeDocument(Document document, StreamResult streamResult, String encoding)
			throws RuntimeException {

		String lineSep = System.getProperty(PROP_LINE_SEPARATOR);
		try {

			String docEncoding = document.getInputEncoding();
			if (StringHelper.isEmpty(docEncoding)) {
				docEncoding = encoding;
			}

			if (!lineSep.equals("\n")) { //$NON-NLS-1$
				System.setProperty(PROP_LINE_SEPARATOR, UNIX_LINE_SEP);
			}

			// Set up a transformer
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer transformer = transfac.newTransformer();
			transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.INDENT, "yes"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.METHOD, "xml"); //$NON-NLS-1$
			transformer.setOutputProperty(OutputKeys.ENCODING, docEncoding);
			transformer.setOutputProperty("{http://xml.apache.org/xalan}indent-amount",
					"2"); //$NON-NLS-1$ //$NON-NLS-2$
			// transformer.setOutputProperty("{http://xml.apache.org/xalan}line-separator", "\t");

			// Transform to file
			Source xmlSource = new DOMSource(document);
			transformer.transform(xmlSource, streamResult);

		} catch (Exception e) {

			throw new XmlException("Exception while exporting to file: " + e, e); //$NON-NLS-1$

		} finally {

			System.setProperty(PROP_LINE_SEPARATOR, lineSep);
		}
	}

	/**
	 * Returns a new document instance
	 *
	 * @return a new document instance
	 *
	 * @throws RuntimeException
	 * 		if something went wrong while creating the XML configuration
	 */
	public static Document createDocument() throws RuntimeException {
		try {

			DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = dbfac.newDocumentBuilder();

			return docBuilder.newDocument();

		} catch (DOMException | ParserConfigurationException e) {
			throw new XmlException("Failed to create Document: " + e.getLocalizedMessage(), e); //$NON-NLS-1$
		}
	}

	public static void marshallTempFile(File tempPath, String prefix, Set<TempFileOptions> options, Object object)
			throws Exception {
		marshall(getTempFile(tempPath, prefix, ".xml", options), object);
	}

	/**
	 * Marshalls the given element annotated with xml annotations to the given destination file
	 *
	 * @param dstFile
	 * 		the destination to marshall the object
	 * @param object
	 * 		the object to marshall
	 *
	 * @throws Exception
	 * 		if the marshalling fails for any reason
	 */
	public static void marshall(File dstFile, Object object) throws Exception {

		try (FileOutputStream out = new FileOutputStream(dstFile)) {

			JAXBContext jc = JAXBContext.newInstance(object.getClass());

			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document document = db.newDocument();

			Marshaller marshaller = jc.createMarshaller();
			marshaller.marshal(object, document);

			writeDocument(document, out);

			out.flush();
		}

		logger.info("Marshalled " + object.getClass() + " to " + dstFile);
	}
}
