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
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PathBuilder {

	private static final Logger logger = LoggerFactory.getLogger(PathBuilder.class);

	public static final String FILE_EXT = ".xml"; //$NON-NLS-1$
	public static final int EXT_LENGTH = PathBuilder.FILE_EXT.length();

	private final String basePath;

	public PathBuilder(String realm, Properties properties) {

		// get properties
		String context = PathBuilder.class.getSimpleName();
		String basePath = PropertiesHelper.getProperty(properties, context, PersistenceConstants.PROP_BASEPATH, null);

		// validate base path exists and is writable
		File basePathF = new File(basePath);
		if (!basePathF.exists())
			throw new XmlPersistenceException(MessageFormat.format("The database store path does not exist at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));
		if (!basePathF.canWrite())
			throw new XmlPersistenceException(MessageFormat.format("The database store path is not writeable at {0}", //$NON-NLS-1$
					basePathF.getAbsolutePath()));

		File realmPathF = new File(basePathF, realm);
		if (!realmPathF.exists() && !realmPathF.mkdir())
			throw new XmlPersistenceException(MessageFormat.format("Could not create path for realm {0} at {1}", //$NON-NLS-1$
					realm, basePathF.getAbsolutePath()));

		// we want a clean base path
		String canonicalBasePath;
		try {
			canonicalBasePath = realmPathF.getCanonicalPath();
		} catch (IOException e) {
			throw new XmlPersistenceException(MessageFormat.format(
					"Failed to build canonical path from {0}", realmPathF.getAbsolutePath()), e); //$NON-NLS-1$
		}

		// this.basePathF = basePathF;
		this.basePath = canonicalBasePath;
		logger.info(MessageFormat.format("Using base path {0}", this.basePath)); //$NON-NLS-1$
	}

	String getFilename(String id) {
		return id.concat(PathBuilder.FILE_EXT);
	}

	String getPathAsString(String type, String subType, String id) {
		StringBuilder sb = new StringBuilder(this.basePath);
		if (!StringHelper.isEmpty(type)) {
			sb.append(File.separatorChar);
			sb.append(type);
		}
		if (!StringHelper.isEmpty(subType)) {
			sb.append(File.separatorChar);
			sb.append(subType);
		}
		if (!StringHelper.isEmpty(id)) {
			sb.append(File.separatorChar);
			sb.append(getFilename(id));
		}

		return sb.toString();
	}

	public File getRootPath() {
		File path = new File(getPathAsString(null, null, null));
		return path;
	}

	public File getTypePath(String type) {
		File path = new File(getPathAsString(type, null, null));
		return path;
	}

	public File getSubTypePath(String type, String subType) {
		File path = new File(getPathAsString(type, subType, null));
		return path;
	}

	public File getIdOfTypePath(String type, String id) {
		File path = new File(getPathAsString(type, null, id));
		return path;
	}

	public File getIdOfSubTypePath(String type, String subType, String id) {
		File path = new File(getPathAsString(type, subType, id));
		return path;
	}
}
