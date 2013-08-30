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
package ch.eitchnet.xmlpers.impl;

import java.io.File;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceFileDao {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceFileDao.class);

	private boolean verbose;
	private XmlPersistencePathBuilder pathBuilder;

	public XmlPersistenceFileDao(XmlPersistencePathBuilder pathBuilder, Properties properties) {

		// get properties
		String context = XmlPersistencePathBuilder.class.getSimpleName();
		boolean verbose = PropertiesHelper.getPropertyBool(properties, context, XmlPersistenceConstants.PROP_VERBOSE,
				Boolean.FALSE).booleanValue();

		// initialize
		this.verbose = verbose;
		this.pathBuilder = pathBuilder;
	}

	/**
	 * @return the pathBuilder
	 */
	public XmlPersistencePathBuilder getPathBuilder() {
		return this.pathBuilder;
	}

	public void removeAll() {

		File removePath = this.pathBuilder.getRemovePath();
		String failMsg = "Deletion of persistence units at {1} failed! Check file permissions!";
		remove(removePath, failMsg, removePath);
	}
	
	public void removeAll(String type) {

		File removePath = this.pathBuilder.getRemovePath(type);
		String failMsg = "Deletion of persistence units for {0} at {1} failed! Check file permissions!";
		remove(removePath, failMsg, type, removePath);
	}

	public void removeAll(String type, String subType) {

		File removePath = this.pathBuilder.getRemovePath(type, subType);
		String failMsg = "Deletion of persistence units for {0} / {1} at {2} failed! Check file permissions!";
		remove(removePath, failMsg, type, subType, removePath);
	}

	public void remove(String type, String id) {
		File removePath = this.pathBuilder.getRemovePath(type, id);
		String failMsg = "Deletion of persistence units for {0} / {1} at {2} failed! Check file permissions!";
		remove(removePath, failMsg, type, id, removePath);
	}

	public void remove(String type, String subType, String id) {
		File removePath = this.pathBuilder.getRemovePath(type, subType, id);
		String failMsg = "Deletion of persistence units for {0} / {1} / {2} at {3} failed! Check file permissions!";
		remove(removePath, failMsg, type, subType, id, removePath);
	}

	private void remove(File removePath, String failMsg, Object... msgParts) {
		File[] removePaths = new File[] { removePath };
		boolean removed = FileHelper.deleteFiles(removePaths, this.verbose);
		if (!removed) {
			String msg = MessageFormat.format(failMsg, msgParts);
			throw new XmlPersistenceException(msg);
		}
	}

	/**
	 * Returns the set of types
	 * 
	 * @return
	 */
	public Set<String> queryKeySet() {
		File queryPath = this.pathBuilder.getQueryPath();
		Set<String> keySet = queryKeySet(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + keySet.size() + " types");
		return keySet;
	}

	/**
	 * Returns the set of sub types for the given type
	 * 
	 * @param type
	 * 
	 * @return
	 */
	public Set<String> queryKeySet(String type) {
		File queryPath = this.pathBuilder.getQueryPath(type);
		Set<String> keySet = queryKeySet(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + keySet.size() + " elements for " + type);
		return keySet;
	}

	/**
	 * Returns the set of ids for the objects with the give type and sub type
	 * 
	 * @param type
	 * @param subType
	 * 
	 * @return
	 */
	public Set<String> queryKeySet(String type, String subType) {
		File queryPath = this.pathBuilder.getQueryPath(type, subType);
		Set<String> keySet = queryKeySet(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + keySet.size() + " elements for " + type);
		return keySet;
	}

	/**
	 * @param queryPath
	 * @return
	 */
	private Set<String> queryKeySet(File queryPath) {
		Set<String> keySet = new HashSet<String>();

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isFile())
				keySet.add(this.pathBuilder.getId(subTypeFile.getName()));
		}

		return keySet;
	}

	public long querySize() {
		File queryPath = this.pathBuilder.getQueryPath();
		long numberOfFiles = querySize(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + numberOfFiles + " types");
		return numberOfFiles;
	}

	public long querySize(String type) {
		File queryPath = this.pathBuilder.getQueryPath(type);
		long numberOfFiles = querySize(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + numberOfFiles + " elements for " + type);
		return numberOfFiles;
	}

	public long querySize(String type, String subType) {
		File queryPath = this.pathBuilder.getQueryPath(type, subType);
		long numberOfFiles = querySize(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + numberOfFiles + " elements for " + type);
		return numberOfFiles;
	}

	private long querySize(File queryPath) {
		long numberOfFiles = 0l;

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile())
				numberOfFiles++;
		}
		return numberOfFiles;
	}
}
