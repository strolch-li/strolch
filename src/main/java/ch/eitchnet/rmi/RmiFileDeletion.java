/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.rmi;

import java.io.Serializable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RmiFileDeletion implements Serializable {

	//
	private static final long serialVersionUID = 1L;

	private String fileName;
	private String fileType;

	/**
	 * @param fileName
	 *            the name of the file to be deleted. This is either just the name, or a path relative to the type
	 * @param fileType
	 *            the type of file to delete. This defines in which path the file resides
	 */
	public RmiFileDeletion(String fileName, String fileType) {
		this.fileName = fileName;
		this.fileType = fileType;
	}

	/**
	 * @return the fileType
	 */
	public String getFileType() {
		return fileType;
	}

	/**
	 * @return the fileName
	 */
	public String getFileName() {
		return fileName;
	}
}
