/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import ch.eitchnet.utils.helper.FileHelper;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;

/**
 *@author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlFilePersister {

	//
	private static final String XML_DEFAULT_ENCODING = "UTF-8";

	private static final Logger logger = LoggerFactory.getLogger(XmlFilePersister.class);

	private boolean verbose;
	private XmlPersistencePathBuilder xmlPathHelper;

	/**
	 * @param xmlPathHelper
	 * @param verbose
	 */
	public XmlFilePersister(XmlPersistencePathBuilder xmlPathHelper, boolean verbose) {
		this.xmlPathHelper = xmlPathHelper;
		this.verbose = verbose;
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 * @param document
	 */
	public void saveOrUpdate(String type, String subType, String id, Document document) {

		File pathF;
		if (subType != null)
			pathF = this.xmlPathHelper.getPathF(type, subType, id);
		else
			pathF = this.xmlPathHelper.getPathF(type, id);

		// if this is a new file, then check create parents, if the don't exist
		if (!pathF.exists()) {
			File parentFile = pathF.getParentFile();
			if (!parentFile.exists() && !parentFile.mkdirs()) {
				throw new XmlPersistenceExecption("Could not create path for " + type + " / " + subType + " / " + id
						+ " at " + pathF.getAbsolutePath());
			}
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Persisting " + type + " / " + subType + " / " + id + " to " + pathF.getAbsolutePath() + "...");

		BufferedOutputStream outStream = null;
		try {

			outStream = new BufferedOutputStream(new FileOutputStream(pathF));

			OutputFormat outputFormat = new OutputFormat("XML", XmlFilePersister.XML_DEFAULT_ENCODING, true);
			outputFormat.setIndent(1);
			outputFormat.setIndenting(true);
			//of.setDoctype(null, null);

			XMLSerializer serializer = new XMLSerializer(outStream, outputFormat);
			serializer.asDOMSerializer();
			serializer.serialize(document);
			outStream.flush();

		} catch (Exception e) {
			throw new XmlPersistenceExecption("Could not persist object " + type + " / " + subType + " / " + id
					+ " to " + pathF.getAbsolutePath(), e);
		} finally {
			if (outStream != null) {
				try {
					outStream.close();
				} catch (IOException e) {
					XmlFilePersister.logger.error(e.getMessage(), e);
				}
			}
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Done.");
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 */
	public void remove(String type, String subType, String id) {

		File pathF;
		if (subType != null)
			pathF = this.xmlPathHelper.getPathF(type, subType, id);
		else
			pathF = this.xmlPathHelper.getPathF(type, id);

		if (this.verbose)
			XmlFilePersister.logger.info("Remove persistence file for " + type + " / " + subType + " / " + id + " from "
					+ pathF.getAbsolutePath() + "...");

		if (!pathF.exists()) {
			XmlFilePersister.logger.error("Persistence file for " + type + " / " + subType + " / " + id + " does not exist at "
					+ pathF.getAbsolutePath());
		} else if (!pathF.delete()) {
			throw new XmlPersistenceExecption("Could not delete persistence file for " + type + " / " + subType + " / "
					+ id + " at " + pathF.getAbsolutePath());
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Done.");
	}

	/**
	 * @param type
	 * @param subType
	 */
	public void removeAll(String type, String subType) {

		File pathF;
		if (subType == null)
			pathF = this.xmlPathHelper.getPathF(type);
		else
			pathF = this.xmlPathHelper.getPathF(type, subType);

		if (!pathF.exists()) {
			if (subType == null)
				XmlFilePersister.logger.error("Path for " + type + " at " + pathF.getAbsolutePath()
						+ " does not exist, so removing not possible!");
			else
				XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
						+ " does not exist, so removing not possible!");

		} else {

			File[] filesToRemove = pathF.listFiles();
			boolean removed = FileHelper.deleteFiles(filesToRemove, this.verbose);

			if (!removed) {
				if (subType == null)
					throw new XmlPersistenceExecption("Could not delete persistence files for " + type + " at "
							+ pathF.getAbsolutePath());

				throw new XmlPersistenceExecption("Could not delete persistence files for " + type + " / " + subType
						+ " at " + pathF.getAbsolutePath());
			}
		}
	}

	/**
	 * 
	 * @param type
	 * @param subType
	 * 
	 * @return
	 */
	public Set<String> queryKeySet(String type, String subType) {

		// if a sub type is required, then it's simple:
		if (subType != null) {

			File pathF = this.xmlPathHelper.getPathF(type, subType);
			if (!pathF.exists()) {
				XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
						+ " does not exist, so no objects exist!");
				return Collections.emptySet();
			}

			Set<String> keySet = new HashSet<String>();
			for (File f : pathF.listFiles()) {
				String name = f.getName();
				keySet.add(name.substring(0, name.length() - XmlPersistencePathBuilder.FILE_EXT.length()));
			}

			if (this.verbose)
				XmlFilePersister.logger.info("Found " + keySet.size() + " elements for " + type + " / " + subType);

			return keySet;
		}

		// otherwise we need to iterate any existing subTypes and create a combined key set
		File pathF = this.xmlPathHelper.getPathF(type);
		if (!pathF.exists()) {
			XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
					+ " does not exist, so no objects exist!");
			return Collections.emptySet();
		}

		Set<String> keySet = new HashSet<String>();

		File[] subTypeFiles = pathF.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile()) {
				keySet.add(this.xmlPathHelper.getId(subTypeFile.getName()));
			} else {

				for (File f : subTypeFile.listFiles()) {
					keySet.add(this.xmlPathHelper.getId(f.getName()));
				}
			}
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Found " + keySet.size() + " elements for " + type);

		return keySet;
	}

	/**
	 * 
	 * @param type
	 * @param subType
	 * 
	 * @return
	 */
	public long querySize(String type, String subType) {

		// if a sub type is required, then it's simple:
		if (subType != null) {

			File pathF = this.xmlPathHelper.getPathF(type, subType);
			if (!pathF.exists()) {
				XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
						+ " does not exist, so no objects exist!");
				return 0l;
			}

			int length = pathF.listFiles().length;

			if (this.verbose)
				XmlFilePersister.logger.info("Found " + length + " elements for " + type + " / " + subType);

			return length;
		}

		// otherwise we need to iterate any existing sub types and 
		// return the size of the combined collection

		File pathF = this.xmlPathHelper.getPathF(type);
		if (!pathF.exists()) {
			XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
					+ " does not exist, so no objects exist!");
			return 0l;
		}

		long numberOfFiles = 0l;

		File[] subTypeFiles = pathF.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile()) {
				numberOfFiles++;
			} else {
				numberOfFiles += subTypeFile.listFiles().length;
			}
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Found " + numberOfFiles + " elements for " + type);

		return numberOfFiles;
	}

//  XXX think about allowing paged loading...
//	/**
//	 * @param type
//	 * @param subType
//	 * @param firstResult
//	 * @param maxResults
//	 *
//	 * @return
//	 */
//	public List<Element> queryFrom(String type, String subType, int firstResult, int maxResults) {
//
//		File pathF = this.xmlPathHelper.getPathF(type, subType);
//		if (!pathF.exists()) {
//			logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
//					+ " does not exist, so no objects exist!");
//			return Collections.emptyList();
//		}
//
//		File[] listFiles = pathF.listFiles();
//		Arrays.sort(listFiles, new Comparator<File>() {
//
//			@Override
//			public int compare(File file1, File file2) {
//				return file1.getName().compareTo(file2.getName());
//			}
//		});
//
//		// make sure positions are not illegal
//		int size = listFiles.length;
//		if (firstResult >= size)
//			return Collections.emptyList();
//
//		if ((firstResult + maxResults) > size)
//			maxResults = size - firstResult;
//
//		File[] result = Arrays.copyOfRange(listFiles, firstResult, firstResult + maxResults);
//
//		List<T> list = new ArrayList<T>();
//		for (File f : result) {
//			list.add(loadObject(clazz, f));
//		}
//
//		return list;
//	}

	/**
	 * 
	 * @param type
	 * @param subType
	 * 
	 * @return
	 */
	public List<Element> queryAll(String type, String subType, DocumentBuilder docBuilder) {

		// if a sub type is required, then it's simple:
		if (subType != null) {

			File pathF = this.xmlPathHelper.getPathF(type, subType);
			if (!pathF.exists()) {
				XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
						+ " does not exist, so no objects exist!");
				return Collections.emptyList();
			}

			List<Element> list = new ArrayList<Element>();
			for (File subTypeF : pathF.listFiles()) {
				list.add(parseFile(subTypeF, docBuilder));
			}

			if (this.verbose)
				XmlFilePersister.logger.info("Loaded " + list.size() + " elements for " + type + " / " + subType);

			return list;
		}

		// otherwise we need to iterate any existing sub types and 
		// return those elements as well

		File pathF = this.xmlPathHelper.getPathF(type);
		if (!pathF.exists()) {
			XmlFilePersister.logger.error("Path for " + type + " / " + subType + " at " + pathF.getAbsolutePath()
					+ " does not exist, so no objects exist!");
			return Collections.emptyList();
		}

		List<Element> list = new ArrayList<Element>();

		File[] subTypeFiles = pathF.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile()) {
				list.add(parseFile(subTypeFile, docBuilder));

			} else {
				for (File subTypeF : subTypeFile.listFiles()) {
					list.add(parseFile(subTypeF, docBuilder));
				}
			}
		}

		if (this.verbose)
			XmlFilePersister.logger.info("Loaded " + list.size() + " elements for " + type);

		return list;
	}

	/**
	 * 
	 * @param type
	 * @param subType
	 * @param id
	 * 
	 * @return
	 */
	public Element queryById(String type, String subType, String id, DocumentBuilder docBuilder) {

		File pathF = this.xmlPathHelper.getPathF(type, subType, id);
		if (!pathF.exists()) {
			XmlFilePersister.logger.error("Path for " + type + " / " + subType + " / " + id + " at " + pathF.getAbsolutePath()
					+ " does not exist, so object does not exist!");
			return null;
		}

		return parseFile(pathF, docBuilder);
	}

	/**
	 * @param subTypeF
	 * @return
	 */
	private Element parseFile(File subTypeF, DocumentBuilder docBuilder) {
		try {

			Document document = docBuilder.parse(subTypeF);
			return document.getDocumentElement();

		} catch (SAXException e) {
			throw new XmlPersistenceExecption("Failed to parse file " + subTypeF.getAbsolutePath(), e);
		} catch (IOException e) {
			throw new XmlPersistenceExecption("Failed to read file " + subTypeF.getAbsolutePath(), e);
		}
	}
}
