/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentFactory;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * Helper class for performing XML based tasks using Dom4J
 * 
 * @author rvonburg
 */
public class XmlHelper {

	/**
	 * DEFAULT_ENCODING = "UTF-8" : defines the default UTF-8 encoding expected of XML files
	 */
	public static final String DEFAULT_ENCODING = "UTF-8";

	private static final Logger logger = Logger.getLogger(XmlHelper.class);

	/**
	 * Parses an XML file on the file system using dom4j and returns the resulting {@link Document} object
	 * 
	 * @param xmlFile
	 *            the {@link File} which has the path to the XML file to read
	 * 
	 * @return a {@link Document} object containing the dom4j {@link Element}s of the XML file
	 */
	public static Document parseDocument(File xmlFile) {

		try {

			InputStream inStream = new FileInputStream(xmlFile);

			SAXReader reader = new SAXReader();
			Document document = reader.read(inStream);

			logger.info("Read XML document " + document.getRootElement().getName());
			return document;

		} catch (FileNotFoundException e) {
			throw new PrivilegeException("The XML file does not exist or is not readable: " + xmlFile.getAbsolutePath());
		} catch (DocumentException e) {
			throw new PrivilegeException("the XML file " + xmlFile.getAbsolutePath() + " is not parseable:", e);
		}
	}

	/**
	 * Writes a dom4j {@link Document} to an XML file on the file system
	 * 
	 * @param document
	 *            the {@link Document} to write to the file system
	 * @param file
	 *            the {@link File} describing the path on the file system where the XML file should be written to
	 */
	public static void writeDocument(Document document, File file) {

		logger.info("Exporting document element " + document.getName() + " to " + file.getAbsolutePath());

		OutputStream fileOutputStream = null;

		try {

			fileOutputStream = new FileOutputStream(file);

			String aEncodingScheme = document.getXMLEncoding();
			if (aEncodingScheme == null || aEncodingScheme.isEmpty()) {
				aEncodingScheme = DEFAULT_ENCODING;
			}
			OutputFormat outformat = OutputFormat.createPrettyPrint();
			outformat.setEncoding(aEncodingScheme);
			XMLWriter writer = new XMLWriter(fileOutputStream, outformat);
			writer.write(document);
			writer.flush();

		} catch (Exception e) {

			throw new PrivilegeException("Exception while exporting to file: " + e, e);

		} finally {

			if (fileOutputStream != null) {
				try {
					fileOutputStream.close();
				} catch (IOException e) {
					logger.error("Could not close file output stream: " + e, e);
				}
			}
		}
	}

	/**
	 * Writes a dom4j {@link Element} to an XML file on the file system
	 * 
	 * @param rootElement
	 *            the {@link Element} to write to the file system
	 * @param file
	 *            the {@link File} describing the path on the file system where the XML file should be written to
	 */
	public static void writeElement(Element rootElement, File file) {

		Document document = DocumentFactory.getInstance().createDocument(DEFAULT_ENCODING);
		document.setRootElement(rootElement);
		document.setName(rootElement.getName());

		writeDocument(document, file);
	}
}
