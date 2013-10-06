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
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistencePathBuilder {
	private static final String SLASH = "/"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistencePathBuilder.class);

	public static final String FILE_EXT = ".xml"; //$NON-NLS-1$
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
			throw new XmlPersistenceException(MessageFormat.format("The database store path does not exist at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));
		if (!basePathF.canWrite())
			throw new XmlPersistenceException(MessageFormat.format("The database store path is not writeable at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));

		// we want a clean base path
		String canonicalBasePath;
		try {
			canonicalBasePath = basePathF.getCanonicalPath();
		} catch (IOException e) {
			throw new XmlPersistenceException(
					MessageFormat.format("Failed to build canonical path from {0}", basePath), e); //$NON-NLS-1$
		}

		// this.basePathF = basePathF;
		this.basePath = canonicalBasePath;
		this.verbose = verbose;

		logger.info(MessageFormat.format("Using base path {0}", this.basePath)); //$NON-NLS-1$
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
			sb.append(SLASH);
			sb.append(type);
		}
		if (!StringHelper.isEmpty(subType)) {
			sb.append(SLASH);
			sb.append(subType);
		}
		if (!StringHelper.isEmpty(id)) {
			sb.append(SLASH);
			sb.append(getFilename(id));
		}

		return sb.toString();
	}

	File getAddPath(String type, String id) {
		assertType(type);
		assertId(id);

		File path = new File(getPathAsString(type, null, id));

		// assert path exists
		String msg = "Persistence unit already exists for {0} / {1} at {2}"; //$NON-NLS-1$
		assertPathNotExists(path, msg, type, id, path.getAbsolutePath());

		// check if parent path exists
		msg = "Could not create parent path for {0} / {1} at {2}"; //$NON-NLS-1$
		createMissingParents(path, msg, type, id, path.getAbsolutePath());

		return path;
	}

	File getAddPath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = new File(getPathAsString(type, subType, id));

		// assert path exists
		String msg = "Persistence unit already exists for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
		assertPathNotExists(path, msg, type, subType, id, path.getAbsolutePath());

		// check if parent path exists
		msg = "Could not create parent path for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
		createMissingParents(path, msg, type, subType, id, path.getAbsolutePath());

		return path;
	}

	File getReadPath(String type, String id) {
		assertType(type);
		assertId(id);
		File path = new File(getPathAsString(type, null, id));
		if (this.verbose) {
			String msg = "Query path for {0} / {1} is {2}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, id, path.getAbsolutePath()));
		}
		return path;
	}

	File getReadPath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);
		File path = new File(getPathAsString(type, subType, id));
		if (this.verbose) {
			String msg = "Query path for {0} / {1} / {2} is {3}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}
		return path;
	}

	File getUpdatePath(String type, String id) {
		assertType(type);
		assertId(id);

		File path = new File(getPathAsString(type, null, id));

		if (!path.exists()) {
			String msg = "Persistence unit does not exist for {0} / {1} at {2}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, type, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getUpdatePath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = new File(getPathAsString(type, subType, id));

		if (!path.exists()) {
			String msg = "Persistence unit does not exist for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath(String type, String id) {
		assertType(type);
		assertId(id);

		File path = new File(getPathAsString(type, null, id));
		if (!path.exists()) {
			String msg = "No Persistence units exist for {0} / {1}  at {2}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, type, id, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for {0} / {1} is {2}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getRemovePath(String type, String subType, String id) {
		assertType(type);
		assertSubType(subType);
		assertId(id);

		File path = new File(getPathAsString(type, subType, id));
		if (!path.exists()) {
			String msg = "Persistence unit for {0} / {1} / {2} does not exist at {3}"; //$NON-NLS-1$
			throw new XmlPersistenceException(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		if (this.verbose) {
			String msg = "Remove path for {0} / {1} / {2} is {3}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, subType, id, path.getAbsolutePath()));
		}

		return path;
	}

	File getQueryPath() {
		return new File(getPathAsString(null, null, null));
	}

	File getQueryPath(String type) {
		assertType(type);
		File path = new File(getPathAsString(type, null, null));
		if (this.verbose) {
			String msg = "Query path for {0} is {1}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, path.getAbsolutePath()));
		}
		return path;
	}

	File getQueryPath(String type, String subType) {
		assertType(type);
		assertSubType(subType);
		File path = new File(getPathAsString(type, subType, null));
		if (this.verbose) {
			String msg = "Query path for {0} / {1} is {2}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, type, subType, path.getAbsolutePath()));
		}
		return path;
	}

	private void assertId(String id) {
		if (StringHelper.isEmpty(id))
			throw new XmlPersistenceException(
					"The id can not be empty! An object must always be handled with at least the type and id!"); //$NON-NLS-1$
	}

	private void assertType(String type) {
		if (StringHelper.isEmpty(type))
			throw new XmlPersistenceException(
					"The type can not be empty! An object must always be handled with at least the type and id!"); //$NON-NLS-1$
	}

	private void assertSubType(String subType) {
		if (StringHelper.isEmpty(subType))
			throw new XmlPersistenceException("The subType can not be empty!"); //$NON-NLS-1$
	}

	private void assertFilename(String filename) {
		if (filename.charAt(filename.length() - XmlPersistencePathBuilder.EXT_LENGTH) != '.') {
			String msg = "The filename does not have a . (dot) at index {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, (filename.length() - XmlPersistencePathBuilder.EXT_LENGTH));
			throw new XmlPersistenceException(msg);
		}
	}

	private void assertPathNotExists(File path, String msg, Object... args) {
		if (path.exists()) {
			throw new XmlPersistenceException(MessageFormat.format(msg, args));
		}
	}

	private void createMissingParents(File path, String msg, Object... args) {
		File parentFile = path.getParentFile();
		if (!parentFile.exists() && !parentFile.mkdirs()) {
			throw new XmlPersistenceException(MessageFormat.format(msg, args));
		}
	}

	private void logPath(String operation, File path, PersistenceContext<?> context) {
		if (this.verbose) {
			String msg;
			if (StringHelper.isEmpty(context.getSubType())) {
				msg = "Path for operation {0} for {1} / {2} / is at {3}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, operation, context.getType(), context.getId(), path.getAbsolutePath());
			} else {
				msg = "Path for operation {0} for {1} / {2} / {3} / is at {4}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, operation, context.getType(), context.getSubType(), context.getId(),
						path.getAbsolutePath());
			}
		}
	}

	private void createMissingParents(File path, PersistenceContext<?> context) {
		File parentFile = path.getParentFile();
		if (!parentFile.exists() && !parentFile.mkdirs()) {
			String msg;
			if (StringHelper.isEmpty(context.getSubType())) {
				msg = "Could not create parent path for {0} / {1} / at {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getId(), path.getAbsolutePath());
			} else {
				msg = "Could not create parent path for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getSubType(), context.getId(),
						path.getAbsolutePath());
			}

			throw new XmlPersistenceException(msg);
		}
	}

	private void assertPathExists(File path, PersistenceContext<?> context) {
		if (!path.exists()) {
			String msg;
			if (StringHelper.isEmpty(context.getSubType())) {
				msg = "Persistence unit does not exist for {0} / {1} at {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getId(), path.getAbsolutePath());
			} else {
				msg = "Persistence unit does not exist for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getSubType(), context.getId(),
						path.getAbsolutePath());
			}

			throw new XmlPersistenceException(msg);
		}
	}

	private void assertPathNotExists(File path, PersistenceContext<?> context) {
		if (path.exists()) {
			String msg;
			if (StringHelper.isEmpty(context.getSubType())) {
				msg = "Persistence unit already exists for {0} / {1} at {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getId(), path.getAbsolutePath());
			} else {
				msg = "Persistence unit already exists for {0} / {1} / {2} at {3}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, context.getType(), context.getSubType(), context.getId(),
						path.getAbsolutePath());
			}

			throw new XmlPersistenceException(msg);
		}
	}

	public File getCreatePath(PersistenceContext<?> context) {
		File path = getPath(context);
		logPath("CREATE", path, context); //$NON-NLS-1$
		assertPathNotExists(path, context);
		createMissingParents(path, context);
		return path;
	}

	public File getDeletePath(PersistenceContext<?> context) {
		File path = getPath(context);
		logPath("DELETE", path, context); //$NON-NLS-1$
		assertPathExists(path, context);
		return path;
	}

	public File getUpdatePath(PersistenceContext<?> context) {
		File path = getPath(context);
		logPath("UPDATE", path, context); //$NON-NLS-1$
		assertPathExists(path, context);
		return path;
	}

	public File getReadPath(PersistenceContext<?> context) {
		File path = getPath(context);
		logPath("READ", path, context); //$NON-NLS-1$
		if (!path.exists())
			return null;
		return path;
	}

	private File getPath(PersistenceContext<?> context) {
		File path = new File(getPathAsString(context.getType(), context.getSubType(), context.getId()));
		return path;
	}
}
