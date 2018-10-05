/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.fileserver;

import java.io.Serializable;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class FileDeletion implements Serializable {

	private String fileName;
	private String fileType;

	/**
	 * @param fileName
	 * 		the name of the file to be deleted. This is either just the name, or a path relative to the type
	 * @param fileType
	 * 		the type of file to delete. This defines in which path the file resides
	 */
	public FileDeletion(String fileName, String fileType) {
		this.fileName = fileName;
		this.fileType = fileType;
	}

	/**
	 * @return the fileType
	 */
	public String getFileType() {
		return this.fileType;
	}

	/**
	 * @return the fileName
	 */
	public String getFileName() {
		return this.fileName;
	}
}
