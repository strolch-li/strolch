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
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistencePathBuilder {
	private static final Logger logger = LoggerFactory.getLogger(XmlPersistencePathBuilder.class);

	public static final String FILE_EXT = ".xml";
	public static final int EXT_LENGTH = XmlPersistencePathBuilder.FILE_EXT.length();

	private final boolean verbose;
	private final String basePath;

	public XmlPersistencePathBuilder(Properties properties) {

		// get properties
		String context = XmlPersistencePathBuilder.class.getSimpleName();
		boolean verbose = PropertiesHelper.getPropertyBool(properties, context, XmlPersistenceConstants.PROP_VERBOSE,
				Boolean.FALSE).booleanValue();
		String basePath = PropertiesHelper
				.getProperty(properties, context, XmlPersistenceConstants.PROP_BASEPATH, null);

		// validate base path exists and is writable
		File basePathF = new File(basePath);
		if (!basePathF.exists())
			throw new XmlPersistenceException("The database store path does not exist at "
					+ basePathF.getAbsolutePath());
		if (!basePathF.canWrite())
			throw new XmlPersistenceException("The database store path is not writeable at "
					+ basePathF.getAbsolutePath());

		// we want a clean base path
		String canonicalBasePath;
		try {
			canonicalBasePath = basePathF.getCanonicalPath();
		} catch (IOException e) {
			throw new XmlPersistenceException("Failed to build canonical path from " + basePath, e);
		}

		// this.basePathF = basePathF;
		this.basePath = canonicalBasePath;
		this.verbose = verbose;

		logger.info("Using base path " + basePath);
	}

	String getFilename(String id) {
		return id.concat(XmlPersistencePathBuilder.FILE_EXT);
	}

	String getId(String filename) {
		assertFilename(filename);

		return filename.substring(0, filename.length() - XmlPersistencePathBuilder.EXT_LENGTH);
	}

	String getPathAsString(String type, String subType, String id) {
		StringBuilder sb = new StringBuilder(this.basePath);
		if (!StringHelper.isEmpty(type)) {
			sb.append("/");
			sb.append(type);
		}
		if (!StringHelper.isEmpty(subType)) {
			sb.append("/");
			sb.append(subType);
		}
		if (!StringHelper.isEmpty(id)) {
			sb.append("/");
			sb.append(getFilename(id));
		}

		return sb.toString();
	}

	File getPath() {
		return new File(getPathAsString(null, null, null));
	}

	File getPath(String type) {
		assertType(type);
		return new File(getPathAsString(type, null, null));
	}

	File getPath(String type, String subType) {
		assertType(type);
		assertSubType(subType);
		return new File(getPathAsString(type, subType, null));
	}

	File getPath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);
		return new File(getPathAsString(type, subType, id));
	}

	File getAddPath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = getPath(type, subType, id);

		if (path.exists()) {
			String msg = "Persistence unit already exists for {0} / {1} / {2} at {3}";
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		// check if parent path exists
		if (!path.exists()) {
			File parentFile = path.getParentFile();
			if (!parentFile.exists() && !parentFile.mkdirs()) {
				String msg = "Could not create parent path for {0} / {1} / {2} at {3}";
				throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
			}
		}

		return path;
	}

	File getUpdatePath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = getPath(type, subType, id);

		if (!path.exists()) {
			String msg = "Persistence unit does not exist for {0} / {1} / {2} at {3}";
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath() {

		File path = getPath();
		if (!path.exists()) {
			String msg = "No Persistence units exist at {0}";
			throw new XmlPersistenceException(MessageFormat.format(msg, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for all is {0}...";
			logger.info(MessageFormat.format(msg, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath(String type) {
		assertType(type);

		File path = getPath(type);
		if (!path.exists()) {
			String msg = "No Persistence units exist for {0} at {1}";
			throw new XmlPersistenceException(MessageFormat.format(msg, type, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for {0} is {1}...";
			logger.info(MessageFormat.format(msg, type, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath(String type, String subType) {
		assertType(type);
		assertSubType(subType);

		File path = getPath(type, subType);
		if (!path.exists()) {
			String msg = "No Persistence units exist for {0} / {1}  at {2}";
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for {0} / {1} is {2}...";
			logger.info(MessageFormat.format(msg, type, subType, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = getPath(type, subType, id);
		if (!path.exists()) {
			String msg = "Persistence unit for {0} / {1} / {2} does not exist at {3}";
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for {0} / {1} / {2} is {3}...";
			logger.info(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getQueryPath() {
		return getPath();
	}

	File getQueryPath(String type) {
		assertType(type);
		File path = getPath(type);
		if (this.verbose) {
			String msg = "Query path for {0} is {1}...";
			logger.info(MessageFormat.format(msg, type, path.getAbsolutePath()));
		}
		return path;
	}

	File getQueryPath(String type, String subType) {
		assertType(type);
		assertSubType(subType);
		File path = getPath(type, subType);
		if (this.verbose) {
			String msg = "Query path for {0} / {1} is {2}...";
			logger.info(MessageFormat.format(msg, type, subType, path.getAbsolutePath()));
		}
		return path;
	}

	File getQueryPath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);
		File path = getPath(type, subType, id);
		if (this.verbose) {
			String msg = "Query path for {0} / {1} / {2} is {3}...";
			logger.info(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}
		return path;
	}

	private void assertId(String id) {
		if (StringHelper.isEmpty(id))
			throw new XmlPersistenceException(
					"The id can not be empty! An object must always be handled with at least the type and id!");
	}

	private void assertType(String type) {
		if (StringHelper.isEmpty(type))
			throw new XmlPersistenceException(
					"The type can not be empty! An object must always be handled with at least the type and id!");
	}

	private void assertSubType(String subType) {
		if (StringHelper.isEmpty(subType))
			throw new XmlPersistenceException("The subType can not be empty!");
	}

	private void assertFilename(String filename) {
		if (filename.charAt(filename.length() - XmlPersistencePathBuilder.EXT_LENGTH) != '.')
			throw new XmlPersistenceException("The filename does not have a . at index "
					+ (filename.length() - XmlPersistencePathBuilder.EXT_LENGTH));
	}
}
