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
package ch.eitchnet.fileserver;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * This class handles remote requests of clients to upload or download a file. Uploading a file is done by calling
 * {@link #handleFilePart(FilePart)} and the downloading a file is done by calling {@link #requestFile(FilePart)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FileHandler {

	private static final Logger logger = LoggerFactory.getLogger(FileHandler.class);

	/**
	 * DEF_PART_SIZE = default part size which is set to 1048576 bytes (1 MiB)
	 */
	public static final int MAX_PART_SIZE = 1048576;

	private String basePath;
	private boolean verbose;

	/**
	 * 
	 */
	public FileHandler(String basePath, boolean verbose) {

		File basePathF = new File(basePath);
		if (!basePathF.exists()) {
			String msg = MessageFormat.format("Base Path does not exist {0}", basePathF.getAbsolutePath()); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}
		if (!basePathF.canWrite()) {
			String msg = MessageFormat.format("Can not write to base path {0}", basePathF.getAbsolutePath()); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}

		this.verbose = verbose;
		this.basePath = basePath;
	}

	/**
	 * Method which a client can request part of a file. The server will fill the given {@link FilePart} with a byte
	 * array of the file, with bytes from the file, respecting the desired offset. It is up to the client to call this
	 * method multiple times for the entire file. It is a decision of the concrete implementation how much data is
	 * returned in each part, the client may pass a request, but this is not definitive
	 * 
	 * @param filePart
	 *            the part of the file
	 */
	public FilePart requestFile(FilePart filePart) {

		// validate file name is legal
		String fileName = filePart.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = filePart.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		String fileTypePath = this.basePath + "/" + fileType; //$NON-NLS-1$
		File file = new File(fileTypePath, filePart.getFileName());

		// now evaluate the file exists
		String fileNotFoundMsg = "The file {0} could not be found in the location for files of type {1}"; //$NON-NLS-1$
		if (!file.canRead()) {
			String msg = fileNotFoundMsg;
			msg = MessageFormat.format(msg, fileName, fileType);
			throw new RuntimeException(msg);
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
		if (requestSize > FileHandler.MAX_PART_SIZE) {
			String msg = "The requested part size {0} is greater than the allowed {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, requestSize, MAX_PART_SIZE);
			throw new RuntimeException(msg);
		}

		// validate lengths and offsets
		if (filePart.getFileLength() != fileSize) {
			String msg = "The part request has a file size {0}, but the file is actually {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, filePart.getFileLength(), fileSize);
			throw new RuntimeException(msg);
		} else if (requestOffset > fileSize) {
			String msg = "The requested file part offset {0} is greater than the size of the file {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, requestOffset, fileSize);
			throw new RuntimeException(msg);
		}
		// Otherwise make sure the offset + request length is not larger than the actual file size. 
		// If it is then this is the end part
		else if (requestOffset + requestSize >= fileSize) {

			long remaining = fileSize - requestOffset;

			// update request size to last part of file
			long l = Math.min(requestSize, remaining);

			// this is a fail safe
			if (l > MAX_PART_SIZE) {
				String msg = "Something went wrong. Min of requestSize and remaining is > MAX_PART_SIZE of {0}!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, MAX_PART_SIZE);
				throw new RuntimeException(msg);
			}

			// this is the size of the array we want to return
			requestSize = (int) l;
			filePart.setPartLength(requestSize);
			filePart.setLastPart(true);
		}

		// now read the part of the file and set it as bytes for the file part
		try (FileInputStream fin = new FileInputStream(file);) {

			// position the stream
			long skip = fin.skip(requestOffset);
			if (skip != requestOffset) {
				String msg = MessageFormat.format("Asked to skip {0} but only skipped {1}", requestOffset, skip); //$NON-NLS-1$
				throw new IOException(msg);
			}

			// read the data
			byte[] bytes = new byte[requestSize];
			int read = fin.read(bytes);
			if (read != requestSize) {
				String msg = MessageFormat.format("Asked to read {0} but only read {1}", requestSize, read); //$NON-NLS-1$
				throw new IOException(msg);
			}

			// set the return result
			filePart.setPartBytes(bytes);

		} catch (FileNotFoundException e) {
			String msg = MessageFormat.format(fileNotFoundMsg, fileName, fileType);
			throw new RuntimeException(msg);
		} catch (IOException e) {
			String msg = "There was an error while reading from the file {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, fileName);
			throw new RuntimeException(msg);
		}

		// we are returning the same object as the user gave us, just modified
		if (this.verbose) {
			String msg = "Read {0} for file {1}/{2}"; //$NON-NLS-1$
			String fileSizeS = FileHelper.humanizeFileSize(filePart.getPartBytes().length);
			msg = MessageFormat.format(msg, fileSizeS, fileType, fileName);
			logger.info(msg);
		}
		return filePart;
	}

	/**
	 * Method with which a client can push parts of files to the server. It is up to the client to send as many parts as
	 * needed, the server will write the parts to the associated file
	 * 
	 * @param filePart
	 *            the part of the file
	 */
	public void handleFilePart(FilePart filePart) {

		// validate file name is legal
		String fileName = filePart.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = filePart.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		String fileTypePath = this.basePath + "/" + fileType; //$NON-NLS-1$
		File dstFile = new File(fileTypePath, filePart.getFileName());

		// if the file already exists, then this may not be a start part
		if (filePart.getPartOffset() == 0 && dstFile.exists()) {
			String msg = "The file {0} already exist for type {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, fileName, fileType);
			throw new RuntimeException(msg);
		}

		// write the part
		FileHelper.appendFilePart(dstFile, filePart.getPartBytes());

		// if this is the last part, then validate the hashes
		if (filePart.isLastPart()) {
			String dstFileHash = StringHelper.getHexString(FileHelper.hashFileSha256(dstFile));
			if (!dstFileHash.equals(filePart.getFileHash())) {
				String msg = "Uploading the file {0} failed because the hashes don''t match. Expected: {1} / Actual: {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, filePart.getFileName(), filePart.getFileHash(), dstFileHash);
				throw new RuntimeException(msg);
			}
		}

		if (this.verbose) {
			String msg;
			if (filePart.isLastPart())
				msg = "Wrote {0} for part of file {1}/{2}"; //$NON-NLS-1$
			else
				msg = "Wrote {0} for last part of file {1}/{2}"; //$NON-NLS-1$
			String fileSizeS = FileHelper.humanizeFileSize(filePart.getPartBytes().length);
			msg = MessageFormat.format(msg, fileSizeS, fileType, fileName);
			logger.info(msg);
		}
	}

	/**
	 * Method with which a client can delete files from the server. It only deletes single files if they exist
	 * 
	 * @param fileDeletion
	 *            the {@link FileDeletion} defining the deletion request
	 * 
	 * @return true if the file was deleted, false if the file did not exist
	 * 
	 */
	public boolean deleteFile(FileDeletion fileDeletion) {

		// validate file name is legal
		String fileName = fileDeletion.getFileName();
		validateFileName(fileName);

		// validate type is legal
		String fileType = fileDeletion.getFileType();
		validateFileType(fileType);

		// evaluate the path where the file should reside
		String fileTypePath = this.basePath + "/" + fileType; //$NON-NLS-1$
		File fileToDelete = new File(fileTypePath, fileDeletion.getFileName());

		// delete the file
		boolean deletedFile = FileHelper.deleteFiles(new File[] { fileToDelete }, true);

		String msg;
		if (deletedFile)
			msg = "Deleted file {1}/{2}"; //$NON-NLS-1$
		else
			msg = "Failed to delete file {1}/{2}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, fileType, fileName);
		logger.info(msg);
		return deletedFile;
	}

	/**
	 * Validates that the file name is legal, i.e. not empty or contains references up the tree
	 * 
	 * @param fileName
	 */
	private void validateFileName(String fileName) {

		if (fileName == null || fileName.isEmpty()) {
			throw new RuntimeException("The file name was not given! Can not find a file without a name!"); //$NON-NLS-1$
		} else if (fileName.contains("/")) { //$NON-NLS-1$
			String msg = "The given file name contains illegal characters. The file name may not contain slashes!"; //$NON-NLS-1$
			throw new RuntimeException(msg);
		}
	}

	/**
	 * Validates that the file type is legal, i.e. not empty or contains references up the tree
	 * 
	 * @param fileType
	 */
	private void validateFileType(String fileType) {
		if (fileType == null || fileType.isEmpty()) {
			throw new RuntimeException("The file type was not given! Can not find a file without a type!"); //$NON-NLS-1$
		} else if (fileType.contains("/")) { //$NON-NLS-1$
			String msg = "The given file type contains illegal characters. The file type may not contain slashes!"; //$NON-NLS-1$
			throw new RuntimeException(msg);
		}
	}
}
