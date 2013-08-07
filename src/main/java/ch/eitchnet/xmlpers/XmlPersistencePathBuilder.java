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

import java.io.File;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistencePathBuilder {
	private static final Logger logger = LoggerFactory.getLogger(XmlPersistencePathBuilder.class);

	public static final String FILE_EXT = ".xml";
	public static final int EXT_LENGTH = XmlPersistencePathBuilder.FILE_EXT.length();

	private String basePath;

	/**
	 * @param basePath
	 */
	public XmlPersistencePathBuilder(String basePath) {
		File basePathF = new File(basePath);
		if (!basePathF.exists())
			throw new XmlPersistenceExecption("The database store path does not exist at "
					+ basePathF.getAbsolutePath());
		if (!basePathF.canWrite())
			throw new XmlPersistenceExecption("The database store path is not writeable at "
					+ basePathF.getAbsolutePath());

		try {
			this.basePath = basePathF.getCanonicalPath();
		} catch (IOException e) {
			throw new XmlPersistenceExecption("Failed to build canonical path from " + basePath, e);
		}

		XmlPersistencePathBuilder.logger.info("Using base path " + basePath);
	}

	/**
	 * @param id
	 * @return
	 */
	public String getFilename(String id) {
		return id.concat(XmlPersistencePathBuilder.FILE_EXT);
	}

	/**
	 * @param filename
	 * @return
	 */
	public String getId(String filename) {
		if (filename.charAt(filename.length() - XmlPersistencePathBuilder.EXT_LENGTH) != '.')
			throw new XmlPersistenceExecption("The filename does not have a . at index "
					+ (filename.length() - XmlPersistencePathBuilder.EXT_LENGTH));

		return filename.substring(0, filename.length() - XmlPersistencePathBuilder.EXT_LENGTH);
	}

	/**
	 * @param type
	 * 
	 * @return
	 */
	public String getPath(String type) {

		StringBuilder sb = new StringBuilder(this.basePath);
		sb.append("/");
		sb.append(type);

		return sb.toString();
	}

	/**
	 * @param type
	 * 
	 * @return
	 */
	public File getPathF(String type) {
		return new File(getPath(type));
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public String getPath(String type, String subType) {

		StringBuilder sb = new StringBuilder(this.basePath);
		sb.append("/");
		sb.append(type);
		sb.append("/");
		sb.append(subType);

		return sb.toString();
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public File getPathF(String type, String subType) {
		return new File(getPath(type, subType));
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 * @return
	 */
	public String getPath(String type, String subType, String id) {

		StringBuilder sb = new StringBuilder(this.basePath);
		sb.append("/");
		sb.append(type);
		sb.append("/");
		sb.append(subType);
		sb.append("/");
		sb.append(getFilename(id));

		return sb.toString();
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 * @return
	 */
	public File getPathF(String type, String subType, String id) {
		return new File(getPath(type, subType, id));
	}
}
