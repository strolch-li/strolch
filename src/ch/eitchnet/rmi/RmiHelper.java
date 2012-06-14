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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.rmi.RemoteException;

import org.apache.log4j.Logger;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class RmiHelper {

	private static final Logger logger = Logger.getLogger(RmiHelper.class);

	/**
	 * @param rmiFileClient
	 * @param filePart
	 * @param dstFile
	 */
	public static void downloadFile(RMIFileClient rmiFileClient, RmiFilePart filePart, File dstFile) {

		// here we don't overwrite, the caller must make sure the destination file does not exist
		if (dstFile.exists())
			throw new RuntimeException("The destination file " + dstFile.getAbsolutePath()
					+ " already exists. Delete it first, if you want to overwrite it!");

		try {
			int loops = 0;
			int startLength = filePart.getPartLength();
			while (true) {
				loops += 1;

				// get the next part
				filePart = rmiFileClient.requestFile(filePart);

				// validate length of data
				if (filePart.getPartLength() != filePart.getPartBytes().length)
					throw new RuntimeException("Invalid FilePart. Part length is not as long as the bytes passed "
							+ filePart.getPartLength() + " / " + filePart.getPartBytes().length);

				// validate offset is size of file
				if (filePart.getPartOffset() != dstFile.length()) {
					throw new RuntimeException("The part offset $offset is not at the end of the file "
							+ filePart.getPartOffset() + " / " + dstFile.length());
				}

				// append the part
				FileHelper.appendFilePart(dstFile, filePart.getPartBytes());

				// update the offset
				filePart.setPartOffset(filePart.getPartOffset() + filePart.getPartBytes().length);

				// break if the offset is past the length of the file
				if (filePart.getPartOffset() >= filePart.getFileLength())
					break;
			}
			logger.info(filePart.getFileType() + ": " + filePart.getFileName() + ": Requested " + loops
					+ " parts. StartSize: " + startLength + " EndSize: " + filePart.getPartLength());

			// validate that the offset is at the end of the file
			if (filePart.getPartOffset() != filePart.getFileLength()) {
				throw new RuntimeException("Offset " + filePart.getPartOffset() + " is not at file length "
						+ filePart.getFileLength() + " after reading all the file parts!");
			}

			// now validate hashes
			String dstFileHash = StringHelper.getHexString(FileHelper.hashFileSha256(dstFile));
			if (!dstFileHash.equals(filePart.getFileHash())) {
				throw new RuntimeException("Downloading the file " + filePart.getFileName()
						+ " failed because the hashes don't match. Expected: " + filePart.getFileHash() + " / Actual: "
						+ dstFileHash);
			}

		} catch (Exception e) {
			if (e instanceof RuntimeException)
				throw (RuntimeException) e;
			throw new RuntimeException("Downloading the file " + filePart.getFileName()
					+ " failed because of an underlying exception " + e.getLocalizedMessage());
		}
	}

	/**
	 * @param rmiFileClient
	 * @param srcFile
	 * @param fileType
	 */
	public static void uploadFile(RMIFileClient rmiFileClient, File srcFile, String fileType) {

		// make sure the source file exists
		if (!srcFile.canRead())
			throw new RuntimeException("The source file does not exist at " + srcFile.getAbsolutePath());

		BufferedInputStream inputStream = null;
		try {

			// get the size of the file
			long fileLength = srcFile.length();
			String fileHash = StringHelper.getHexString(FileHelper.hashFileSha256(srcFile));

			// create the file part to send
			RmiFilePart filePart = new RmiFilePart(srcFile.getName(), fileType);
			filePart.setFileLength(fileLength);
			filePart.setFileHash(fileHash);

			// define the normal size of the parts we're sending. The last part will naturally have a different size
			int partLength;
			if (fileLength > RmiFileHandler.MAX_PART_SIZE)
				partLength = RmiFileHandler.MAX_PART_SIZE;
			else
				partLength = (int) fileLength;

			// this is the byte array of data we're sending each time
			byte[] bytes = new byte[partLength];

			// open the stream to the file
			inputStream = new BufferedInputStream(new FileInputStream(srcFile));

			int read = 0;
			int offset = 0;

			// loop by reading the number of bytes needed for each part
			int loops = 0;
			int startLength = partLength;
			while (true) {
				loops += 1;

				// read the bytes into the array
				read = inputStream.read(bytes);

				// validate we read the expected number of bytes
				if (read == -1)
					throw new IOException("Something went wrong while reading the bytes as -1 was returned!");
				if (read != bytes.length) {
					throw new IOException(
							"Something went wrong while reading the bytes as the wrong number of bytes were read. Expected "
									+ bytes.length + " Actual: " + read);
				}

				// set the fields on the FilePart
				filePart.setPartBytes(bytes);
				filePart.setPartOffset(offset);
				filePart.setPartLength(bytes.length);

				// and if this is the last part, then also set that
				int nextOffset = offset + bytes.length;
				if (nextOffset == fileLength)
					filePart.setLastPart(true);

				// now send the part to the server
				rmiFileClient.uploadFilePart(filePart);

				// if this was the last part, then break
				if (filePart.isLastPart())
					break;

				// otherwise update the offset for the next part by also making sure the next part is not larger than
				// the last part of the file
				if (nextOffset + bytes.length > fileLength) {
					long remaining = fileLength - nextOffset;
					if (remaining > RmiFileHandler.MAX_PART_SIZE)
						throw new RuntimeException("Something went wrong as the remaining part " + remaining
								+ " is larger than MAX_PART_SIZE " + RmiFileHandler.MAX_PART_SIZE + "!");
					partLength = (int) remaining;
					bytes = new byte[partLength];
				}

				// and save the offset for the next loop
				offset = nextOffset;
			}

			logger.info(filePart.getFileType() + ": " + filePart.getFileName() + ": Sent " + loops
					+ " parts. StartSize: " + startLength + " EndSize: " + filePart.getPartLength());

		} catch (Exception e) {
			if (e instanceof RuntimeException)
				throw (RuntimeException) e;
			throw new RuntimeException("Uploading the file " + srcFile.getAbsolutePath()
					+ " failed because of an underlying exception " + e.getLocalizedMessage());
		} finally {
			if (inputStream != null) {
				try {
					inputStream.close();
				} catch (IOException e) {
					logger.error("Exception while closing FileInputStream " + e.getLocalizedMessage());
				}
			}
		}
	}

	/**
	 * @param rmiFileClient
	 * @param fileDeletion
	 * @param dstFile
	 */
	public static void deleteFile(RMIFileClient rmiFileClient, RmiFileDeletion fileDeletion, File dstFile) {

		try {
			rmiFileClient.deleteFile(fileDeletion);
		} catch (RemoteException e) {
			throw new RuntimeException("Deleting the file " + fileDeletion.getFileName()
					+ " failed because of an underlying exception " + e.getLocalizedMessage());
		}
	}
}
