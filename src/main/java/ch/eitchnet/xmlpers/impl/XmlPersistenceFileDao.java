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
import java.util.Collections;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
	 * Does a recursive and complete deletion of all objects, types and sub-types
	 */
	public void removeAll() {

		Set<String> types = queryTypeSet();
		for (String type : types) {

			Set<String> idsByType = queryKeySet(type);
			for (String id : idsByType) {
				remove(type, id);
			}

			Set<String> subTypes = queryTypeSet(type);
			for (String subType : subTypes) {

				Set<String> idsBySubType = queryKeySet(type, subType);
				for (String id : idsBySubType) {
					remove(type, subType, id);
				}
			}
		}
	}

	public void removeAll(String type) {

		Set<String> idsByType = queryKeySet(type);
		for (String id : idsByType) {
			remove(type, id);
		}

		Set<String> subTypes = queryTypeSet(type);
		for (String subType : subTypes) {

			Set<String> idsBySubType = queryKeySet(type, subType);
			for (String id : idsBySubType) {
				remove(type, subType, id);
			}
		}
	}

	public void removeAll(String type, String subType) {

		Set<String> idsBySubType = queryKeySet(type, subType);
		for (String id : idsBySubType) {
			remove(type, subType, id);
		}
	}

	public void remove(String type, String id) {
		File removePath = this.pathBuilder.getRemovePath(type, id);
		String failMsg = "Deletion of persistence units for {0} / {1} at {2} failed! Check file permissions!";
		remove(removePath, failMsg, type, id, removePath);

		// if no more objects with this type exist, then delete the path
		failMsg = "Deletion of empty directory for {0} at {2} failed! Check file permissions!";
		File parentFile = removePath.getParentFile();
		deleteEmptyDirectory(parentFile, failMsg, type, parentFile);
	}

	public void remove(String type, String subType, String id) {
		File removePath = this.pathBuilder.getRemovePath(type, subType, id);
		String failMsg = "Deletion of persistence units for {0} / {1} / {2} at {3} failed! Check file permissions!";
		remove(removePath, failMsg, type, subType, id, removePath);

		// if no more objects with this subType exist, then delete the path
		failMsg = "Deletion of empty directory for {0} / {1} at {2} failed! Check file permissions!";
		File parentFile = removePath.getParentFile();
		deleteEmptyDirectory(parentFile, failMsg, type, subType, parentFile);
	}

	public File getAddPath(String type, String id) {
		return this.pathBuilder.getAddPath(type, id);
	}

	public File getAddPath(String type, String subType, String id) {
		return this.pathBuilder.getAddPath(type, subType, id);
	}

	public File getReadPath(String type, String id) {
		return this.pathBuilder.getReadPath(type, id);
	}

	public File getReadPath(String type, String subType, String id) {
		return this.pathBuilder.getReadPath(type, subType, id);
	}

	public File getUpdatePath(String type, String id) {
		return this.pathBuilder.getUpdatePath(type, id);
	}

	public File getUpdatePath(String type, String subType, String id) {
		return this.pathBuilder.getUpdatePath(type, subType, id);
	}

	/**
	 * Returns the set of types
	 * 
	 * @return
	 */
	public Set<String> queryTypeSet() {
		File queryPath = this.pathBuilder.getQueryPath();
		Set<String> keySet = queryTypeSet(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + keySet.size() + " types");
		return keySet;
	}

	/**
	 * Returns the set of sub types for the given type
	 * 
	 * @return
	 */
	public Set<String> queryTypeSet(String type) {
		File queryPath = this.pathBuilder.getQueryPath(type);
		Set<String> keySet = queryTypeSet(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + keySet.size() + " subTypes of type " + type);
		return keySet;
	}

	/**
	 * Returns the object ids for the given type
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
	 * Returns the object ids for the give type and sub type
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

	public long queryTypeSize() {
		File queryPath = this.pathBuilder.getQueryPath();
		long numberOfFiles = queryTypeSize(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + numberOfFiles + " types");
		return numberOfFiles;
	}

	public long queryTypeSize(String type) {
		File queryPath = this.pathBuilder.getQueryPath(type);
		long numberOfFiles = queryTypeSize(queryPath);
		if (this.verbose)
			XmlPersistenceFileDao.logger.info("Found " + numberOfFiles + " elements for " + type);
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

	/**
	 * Returns the types, i.e. directories in the given query path
	 * 
	 * @param queryPath
	 *            the path for which the types should be gathered
	 * 
	 * @return a set of types in the given query path
	 */
	private Set<String> queryTypeSet(File queryPath) {
		Set<String> keySet = new HashSet<String>();

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isFile()) {
				String filename = subTypeFile.getName();
				String id = this.pathBuilder.getId(filename);
				keySet.add(id);
			}
		}

		return keySet;
	}

	/**
	 * Returns the ids of all objects in the given query path, i.e. the id part of all the files in the given query path
	 * 
	 * @param queryPath
	 *            the path for which the ids should be gathered
	 * 
	 * @return a set of ids for the objects in the given query path
	 */
	private Set<String> queryKeySet(File queryPath) {
		if (!queryPath.exists())
			return Collections.emptySet();
		if (!queryPath.isDirectory())
			throw new IllegalArgumentException("The path is not a directory, thus can not query key set for it: "
					+ queryPath.getAbsolutePath());

		Set<String> keySet = new HashSet<String>();

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {
			if (subTypeFile.isFile()) {
				String filename = subTypeFile.getName();
				String id = this.pathBuilder.getId(filename);
				keySet.add(id);
			}
		}

		return keySet;
	}

	/**
	 * Returns the number of all types, i.e. directories in the given query path
	 * 
	 * @param queryPath
	 *            the path in which to count the types
	 * 
	 * @return the number of types in the given query path
	 */
	private long queryTypeSize(File queryPath) {
		if (!queryPath.exists())
			return 0L;
		if (!queryPath.isDirectory())
			throw new IllegalArgumentException("The path is not a directory, thus can not query type size for it: "
					+ queryPath.getAbsolutePath());

		long numberOfFiles = 0l;

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isDirectory())
				numberOfFiles++;
		}
		return numberOfFiles;
	}

	/**
	 * Returns the number of all objects in the given query path
	 * 
	 * @param queryPath
	 *            the path in which to count the objects
	 * 
	 * @return the number of objects in the given query path
	 */
	private long querySize(File queryPath) {
		if (!queryPath.exists())
			return 0L;
		if (!queryPath.isDirectory())
			throw new IllegalArgumentException("The path is not a directory, thus can not query key size for it: "
					+ queryPath.getAbsolutePath());

		long numberOfFiles = 0l;

		File[] subTypeFiles = queryPath.listFiles();
		for (File subTypeFile : subTypeFiles) {

			if (subTypeFile.isFile())
				numberOfFiles++;
		}
		return numberOfFiles;
	}

	private void remove(File removePath, String failMsg, Object... msgParts) {
		if (!removePath.isFile())
			throw new IllegalArgumentException("The given path for deletion is not a file:"
					+ removePath.getAbsolutePath());
		if (!removePath.delete()) {
			String msg = MessageFormat.format(failMsg, msgParts);
			throw new XmlPersistenceException(msg);
		}
	}

	private void deleteEmptyDirectory(File directoryPath, String failMsg, Object... msgArgs) {
		if (!directoryPath.isDirectory())
			throw new IllegalArgumentException("The given path for deletion when empty is not a directory:"
					+ directoryPath.getAbsolutePath());
		if (directoryPath.list().length == 0) {
			if (this.verbose) {
				String msg = "Deleting empty directory for type {0} at {1}";
				logger.info(MessageFormat.format(msg, directoryPath.getName(), directoryPath));
			}
			if (!directoryPath.delete()) {
				String msg = MessageFormat.format(failMsg, msgArgs);
				throw new XmlPersistenceException(msg);
			}
		}
	}
}
