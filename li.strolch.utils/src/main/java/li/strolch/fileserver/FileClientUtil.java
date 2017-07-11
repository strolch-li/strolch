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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.rmi.RemoteException;
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 * 
 */
public class FileClientUtil {

	private static final Logger logger = LoggerFactory.getLogger(FileClientUtil.class);

	public static void downloadFile(FileClient rmiFileClient, FilePart origFilePart, File dstFile) {

		// here we don't overwrite, the caller must make sure the destination file does not exist
		if (dstFile.exists()) {
			String msg = "The destination file {0} already exists. Delete it first, if you want to overwrite it!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, dstFile.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		try {

			FilePart tmpPart = origFilePart;

			int loops = 0;
			int startLength = tmpPart.getPartLength();
			while (true) {
				loops += 1;

				// get the next part
				tmpPart = rmiFileClient.requestFile(tmpPart);

				// validate length of data
				if (tmpPart.getPartLength() != tmpPart.getPartBytes().length) {
					String msg = "Invalid tmpPart. Part length is not as long as the bytes passed {0} / {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, tmpPart.getPartLength(), tmpPart.getPartBytes().length);
					throw new RuntimeException(msg);
				}

				// validate offset is size of file
				if (tmpPart.getPartOffset() != dstFile.length()) {
					String msg = "The part offset $offset is not at the end of the file {0} / {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, tmpPart.getPartOffset(), dstFile.length());
					throw new RuntimeException(msg);
				}

				// append the part
				FileHelper.appendFilePart(dstFile, tmpPart.getPartBytes());

				// update the offset
				tmpPart.setPartOffset(tmpPart.getPartOffset() + tmpPart.getPartBytes().length);

				// break if the offset is past the length of the file
				if (tmpPart.getPartOffset() >= tmpPart.getFileLength())
					break;
			}

			String msg = "{0}: {1}: Requested {2} parts. StartSize: {3} EndSize: {4}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, tmpPart.getFileType(), tmpPart.getFileName(), loops, startLength,
					tmpPart.getPartLength());
			logger.info(msg);

			// validate that the offset is at the end of the file
			if (tmpPart.getPartOffset() != origFilePart.getFileLength()) {
				msg = "Offset {0} is not at file length {1} after reading all the file parts!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, tmpPart.getPartOffset(), origFilePart.getFileLength());
				throw new RuntimeException(msg);
			}

			// now validate hashes
			String dstFileHash = StringHelper.toHexString(FileHelper.hashFileSha256(dstFile));
			if (!dstFileHash.equals(origFilePart.getFileHash())) {
				msg = "Downloading the file {0} failed because the hashes don''t match. Expected: {1} / Actual: {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, origFilePart.getFileName(), origFilePart.getFileHash(), dstFileHash);
				throw new RuntimeException(msg);
			}

		} catch (Exception e) {
			if (e instanceof RuntimeException)
				throw (RuntimeException) e;
			String msg = "Downloading the file {0} failed because of an underlying exception {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, origFilePart.getFileName(), e.getLocalizedMessage());
			throw new RuntimeException(msg);
		}
	}

	public static void uploadFile(FileClient rmiFileClient, File srcFile, String fileType) {

		// make sure the source file exists
		if (!srcFile.canRead()) {
			String msg = MessageFormat.format("The source file does not exist at {0}", srcFile.getAbsolutePath()); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}

		try (BufferedInputStream inputStream = new BufferedInputStream(Files.newInputStream(srcFile.toPath()))) {

			// get the size of the file
			long fileLength = srcFile.length();
			String fileHash = StringHelper.toHexString(FileHelper.hashFileSha256(srcFile));

			// create the file part to send
			FilePart filePart = new FilePart(srcFile.getName(), fileType);
			filePart.setFileLength(fileLength);
			filePart.setFileHash(fileHash);

			// define the normal size of the parts we're sending. The last part will naturally have a different size
			int partLength;
			if (fileLength > FileHandler.MAX_PART_SIZE)
				partLength = FileHandler.MAX_PART_SIZE;
			else
				partLength = (int) fileLength;

			// this is the byte array of data we're sending each time
			byte[] bytes = new byte[partLength];

			// open the stream to the file
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
					throw new IOException("Something went wrong while reading the bytes as -1 was returned!"); //$NON-NLS-1$
				if (read != bytes.length) {
					String msg = "Something went wrong while reading the bytes as the wrong number of bytes were read. Expected {0} Actual: {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, bytes.length, read);
					throw new IOException(msg);
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
					if (remaining > FileHandler.MAX_PART_SIZE) {
						String msg = "Something went wrong as the remaining part {0} is larger than MAX_PART_SIZE {1}!"; //$NON-NLS-1$
						msg = MessageFormat.format(msg, remaining, FileHandler.MAX_PART_SIZE);
						throw new RuntimeException(msg);
					}
					partLength = (int) remaining;
					bytes = new byte[partLength];
				}

				// and save the offset for the next loop
				offset = nextOffset;
			}

			String msg = "{0}: {1}: Sent {2} parts. StartSize: {3} EndSize: {4}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, filePart.getFileType(), filePart.getFileName(), loops, startLength,
					filePart.getPartLength());
			logger.info(msg);

		} catch (Exception e) {
			if (e instanceof RuntimeException)
				throw (RuntimeException) e;
			String msg = "Uploading the file {0} failed because of an underlying exception {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, srcFile.getAbsolutePath(), e.getLocalizedMessage());
			throw new RuntimeException(msg);
		}
	}

	public static void deleteFile(FileClient rmiFileClient, FileDeletion fileDeletion, File dstFile) {

		try {
			rmiFileClient.deleteFile(fileDeletion);
		} catch (RemoteException e) {
			String msg = "Deleting the file {0} failed because of an underlying exception {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, fileDeletion.getFileName(), e.getLocalizedMessage());
			throw new RuntimeException(msg);
		}
	}
}
