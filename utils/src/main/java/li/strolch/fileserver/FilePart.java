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
public class FilePart implements Serializable {

	private String fileName;
	private long fileLength;
	private String fileHash;
	private String fileType;

	private long partOffset;
	private int partLength;
	private byte[] partBytes;
	private boolean lastPart;

	/**
	 * @param fileName
	 * 		the name of the file being referenced. This is either just the name, or a path relative to the type
	 * @param fileType
	 * 		defines the type of file being uploaded or retrieved. This defines in which path the file resides
	 */
	public FilePart(String fileName, String fileType) {

		if (fileName == null || fileName.isEmpty())
			throw new RuntimeException("fileName may not be empty!"); //$NON-NLS-1$
		if (fileType == null || fileType.isEmpty())
			throw new RuntimeException("fileType may not be empty!"); //$NON-NLS-1$

		this.fileName = fileName;
		this.fileType = fileType;

		this.partOffset = 0;
		this.partLength = FileHandler.MAX_PART_SIZE;
		this.partBytes = null;
	}

	/**
	 * @return the fileLength
	 */
	public long getFileLength() {
		return this.fileLength;
	}

	/**
	 * @param fileLength
	 * 		the fileLength to set
	 */
	public void setFileLength(long fileLength) {
		this.fileLength = fileLength;
	}

	/**
	 * @return the fileHash
	 */
	public String getFileHash() {
		return this.fileHash;
	}

	/**
	 * @param fileHash
	 * 		the fileHash to set
	 */
	public void setFileHash(String fileHash) {
		this.fileHash = fileHash;
	}

	/**
	 * @return the fileType
	 */
	public String getFileType() {
		return this.fileType;
	}

	/**
	 * @return the partOffset
	 */
	public long getPartOffset() {
		return this.partOffset;
	}

	/**
	 * @param partOffset
	 * 		the partOffset to set
	 */
	public void setPartOffset(long partOffset) {
		this.partOffset = partOffset;
	}

	/**
	 * @return the partLength
	 */
	public int getPartLength() {
		return this.partLength;
	}

	/**
	 * @param partLength
	 * 		the partLength to set
	 */
	public void setPartLength(int partLength) {
		this.partLength = partLength;
	}

	/**
	 * @return the partBytes
	 */
	public byte[] getPartBytes() {
		return this.partBytes;
	}

	/**
	 * @param partBytes
	 * 		the partBytes to set
	 */
	public void setPartBytes(byte[] partBytes) {
		this.partBytes = partBytes;
	}

	/**
	 * @return the lastPart
	 */
	public boolean isLastPart() {
		return this.lastPart;
	}

	/**
	 * @param lastPart
	 * 		the lastPart to set
	 */
	public void setLastPart(boolean lastPart) {
		this.lastPart = lastPart;
	}

	/**
	 * @return the fileName
	 */
	public String getFileName() {
		return this.fileName;
	}
}
