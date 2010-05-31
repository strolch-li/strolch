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
import java.io.InputStream;

import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.SAXReader;

import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class XmlHelper {

	private static final Logger logger = Logger.getLogger(XmlHelper.class);

	public static Document parseDocument(File xmlFile) {
		try {

			InputStream inStream = new FileInputStream(xmlFile);

			SAXReader reader = new SAXReader();
			Document document = reader.read(inStream);

			logger.info("Read Xml document " + document.getRootElement().getName());
			return document;

		} catch (FileNotFoundException e) {
			throw new PrivilegeException("The Xml file does not exist or is not readable: " + xmlFile.getAbsolutePath());
		} catch (DocumentException e) {
			throw new PrivilegeException("the Xml file " + xmlFile.getAbsolutePath() + " is not parseable:", e);
		}
	}
}
