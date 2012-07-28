/*
 * Copyright (c) 2012
 * 
 * Robert von Burg <eitch@eitchnet.ch>
 * 
 */

/*
 * This file is part of ch.eitchnet.java.utils
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.rmi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.apache.log4j.Logger;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * This class handles remote requests of clients to upload or download a file. Uploading a file is done by calling
 * {@link #handleFilePart(RmiFilePart)} and the downloading a file is done by calling {@link #requestFile(RmiFilePart)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RmiFileHandler {

	private static final Logger logger = Logger.getLogger(RmiFileHandler.class);

	/**
	 * DEF_PART_SIZE = default part size which is set to 1048576 bytes (1 MiB)
	 */
	public static final int MAX_PART_SIZE = 1048576;

	private String basePath;

	/**
	 * 
	 */
	public RmiFileHandler(String basePath) {

		File basePathF = new File(basePath);
		if (!basePathF.exists())
			throw new RuntimeException("Base Path does not exist " + basePathF.getAbsolutePath());
		if (!basePathF.canWrite())
			throw new RuntimeException("Can not write to base path " + basePathF.getAbsolutePath());

		this.basePath = basePath;
	}

	/**
	 * Method which a client can request part of a file. The server will fill the given {@link RmiFilePart} with a byte
	 * array of the file, with bytes from the file, respecting the desired offset. It is up to the client to call this
	 * method multiple times for the entire file. It is a decision of the concrete implementation how much data is
	 * returned in each part, the client may pass a request, but this is not definitive
	 * 
	 * @param filePart
	 *            the part of the file
	 */
	public RmiFilePart requestFile(RmiFilePart filePart) {

		// validate file name is legal
		String fileName = filePart.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = filePart.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		File file = new File(basePath + "/" + fileType, filePart.getFileName());

		// now evaluate the file exists
		if (!file.canRead()) {
			throw new RuntimeException("The file " + fileName
					+ " could not be found in the location for files of type " + fileType);
		}

		// if this is the start of the file, then prepare the file part
		long fileSize = file.length();
		if (filePart.getPartOffset() == 0) {

			// set the file length
			filePart.setFileLength(fileSize);

			// set the SHA256 of the file
			filePart.setFileHash(StringHelper.getHexString(FileHelper.hashFileSha256(file)));
		}

		// variables defining the part of the file we're going to return
		long requestOffset = filePart.getPartOffset();
		int requestSize = filePart.getPartLength();
		if (requestSize > MAX_PART_SIZE) {
			throw new RuntimeException("The requested part size " + requestSize + " is greater than the allowed "
					+ MAX_PART_SIZE);
		}

		// validate lengths and offsets
		if (filePart.getFileLength() != fileSize) {
			throw new RuntimeException("The part request has a file size " + filePart.getFileLength()
					+ ", but the file is actually " + fileSize);
		} else if (requestOffset > fileSize) {
			throw new RuntimeException("The requested file part offset " + requestOffset
					+ " is greater than the size of the file " + fileSize);
		}
		// Otherwise make sure the offset + request length is not larger than the actual file size. 
		// If it is then this is the end part
		else if (requestOffset + requestSize >= fileSize) {

			long remaining = fileSize - requestOffset;

			// update request size to last part of file
			long l = Math.min(requestSize, remaining);

			// this is a fail safe
			if (l > MAX_PART_SIZE)
				throw new RuntimeException("Something went wrong. Min of requestSize and remaining is > MAX_PART_SIZE!");

			// this is the size of the array we want to return
			requestSize = (int) l;
			filePart.setPartLength(requestSize);
			filePart.setLastPart(true);
		}

		// now read the part of the file and set it as bytes for the file part
		FileInputStream fin = null;
		try {

			// position the stream
			fin = new FileInputStream(file);
			long skip = fin.skip(requestOffset);
			if (skip != requestOffset)
				throw new IOException("Asked to skip " + requestOffset + " but only skipped " + skip);

			// read the data
			byte[] bytes = new byte[requestSize];
			int read = fin.read(bytes);
			if (read != requestSize)
				throw new IOException("Asked to read " + requestSize + " but only read " + read);

			// set the return result
			filePart.setPartBytes(bytes);

		} catch (FileNotFoundException e) {
			throw new RuntimeException("The file " + fileName
					+ " could not be found in the location for files of type " + fileType);
		} catch (IOException e) {
			throw new RuntimeException("There was an error while reading from the file " + fileName);
		} finally {
			if (fin != null) {
				try {
					fin.close();
				} catch (IOException e) {
					logger.error("Error while closing FileInputStream: " + e.getLocalizedMessage());
				}
			}
		}

		// we are returning the same object as the user gave us, just edited
		return filePart;
	}

	/**
	 * Method with which a client can push parts of files to the server. It is up to the client to send as many parts as
	 * needed, the server will write the parts to the associated file
	 * 
	 * @param filePart
	 *            the part of the file
	 */
	public void handleFilePart(RmiFilePart filePart) {

		// validate file name is legal
		String fileName = filePart.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = filePart.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		File dstFile = new File(basePath + "/" + fileType, filePart.getFileName());

		// if the file already exists, then this may not be a start part
		if (filePart.getPartOffset() == 0 && dstFile.exists()) {
			throw new RuntimeException("The file " + fileName + " already exist for type " + fileType);
		}

		// write the part
		FileHelper.appendFilePart(dstFile, filePart.getPartBytes());

		// if this is the last part, then validate the hashes
		if (filePart.isLastPart()) {
			String dstFileHash = StringHelper.getHexString(FileHelper.hashFileSha256(dstFile));
			if (!dstFileHash.equals(filePart.getFileHash())) {
				throw new RuntimeException("Uploading the file " + filePart.getFileName()
						+ " failed because the hashes don't match. Expected: " + filePart.getFileHash() + " / Actual: "
						+ dstFileHash);
			}
		}
	}

	/**
	 * Method with which a client can delete files from the server. It only deletes single files if they exist
	 * 
	 * @param fileDeletion
	 *            the {@link RmiFileDeletion} defining the deletion request
	 * 
	 * @return true if the file was deleted, false if the file did not exist
	 * 
	 */
	public boolean deleteFile(RmiFileDeletion fileDeletion) {

		// validate file name is legal
		String fileName = fileDeletion.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = fileDeletion.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		File fileToDelete = new File(basePath + "/" + fileType, fileDeletion.getFileName());

		// delete the file
		return FileHelper.deleteFiles(new File[] { fileToDelete }, true);
	}

	/**
	 * Validates that the file name is legal, i.e. not empty or contains references up the tree
	 * 
	 * @param fileName
	 */
	private void validateFileName(String fileName) {

		if (fileName == null || fileName.isEmpty()) {
			throw new RuntimeException("The file name was not given! Can not find a file without a name!");
		} else if (fileName.contains("/")) {
			throw new RuntimeException(
					"The given file name contains illegal characters. The file name may not contain slashes!");
		}
	}

	/**
	 * Validates that the file type is legal, i.e. not empty or contains references up the tree
	 * 
	 * @param fileType
	 */
	private void validateFileType(String fileType) {
		if (fileType == null || fileType.isEmpty()) {
			throw new RuntimeException("The file type was not given! Can not find a file without a type!");
		} else if (fileType.contains("/")) {
			throw new RuntimeException(
					"The given file type contains illegal characters. The file type may not contain slashes!");
		}
	}
}
