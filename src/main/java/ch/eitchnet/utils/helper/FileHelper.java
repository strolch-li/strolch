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
package ch.eitchnet.utils.helper;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class for dealing with files
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FileHelper {

	private static final Logger logger = LoggerFactory.getLogger(FileHelper.class);

	/**
	 * Reads the contents of a file into a string. Note, no encoding is checked. It is expected to be UTF-8
	 * 
	 * @param file
	 *            the file to read
	 * @return the contents of a file as a string
	 */
	public static final String readFileToString(File file) {

		BufferedReader bufferedReader = null;
		try {

			bufferedReader = new BufferedReader(new FileReader(file));
			StringBuilder sb = new StringBuilder();

			String line;

			while ((line = bufferedReader.readLine()) != null) {
				sb.append(line + "\n");
			}

			return sb.toString();

		} catch (FileNotFoundException e) {
			throw new RuntimeException("Filed does not exist " + file.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("Could not read file " + file.getAbsolutePath());
		} finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				} catch (IOException e) {
					FileHelper.logger.error("Failed to close BufferedReader: " + e.getLocalizedMessage());
				}
			}
		}
	}

	/**
	 * Writes the string to dstFile
	 * 
	 * @param dstFile
	 *            the file to write to
	 */
	public static final void writeStringToFile(String string, File dstFile) {
		try {

			BufferedWriter bufferedwriter = new BufferedWriter(new FileWriter(dstFile));
			bufferedwriter.write(string);

			bufferedwriter.close();

		} catch (FileNotFoundException e) {
			throw new RuntimeException("Filed does not exist " + dstFile.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("Could not write to file " + dstFile.getAbsolutePath());
		}
	}

	/**
	 * Deletes files recursively. No question asked, but logging is done in case of problems
	 * 
	 * @param file
	 *            the file to delete
	 * @param log
	 * @return true if all went well, and false if it did not work. The log will contain the problems encountered
	 */
	public final static boolean deleteFile(File file, boolean log) {
		return FileHelper.deleteFiles(new File[] { file }, log);
	}

	/**
	 * Deletes files recursively. No question asked, but logging is done in case of problems
	 * 
	 * @param files
	 *            the files to delete
	 * @param log
	 * @return true if all went well, and false if it did not work. The log will contain the problems encountered
	 */
	public final static boolean deleteFiles(File[] files, boolean log) {

		boolean worked = true;
		for (int i = 0; i < files.length; i++) {
			File file = files[i];
			if (file.isDirectory()) {
				boolean done = FileHelper.deleteFiles(file.listFiles(), log);
				if (!done) {
					worked = false;
					FileHelper.logger.warn("Could not empty the directory: " + file.getAbsolutePath());
				} else {
					done = file.delete();
					if (done) {
						if (log)
							FileHelper.logger.info("Deleted DIR  " + file.getAbsolutePath());
					} else {
						worked = false;
						FileHelper.logger.warn("Could not delete the directory: " + file.getAbsolutePath());
					}
				}
			} else {
				boolean done = file.delete();
				if (done) {
					if (log)
						FileHelper.logger.info("Deleted FILE " + file.getAbsolutePath());
				} else {
					worked = false;
					FileHelper.logger.warn(("Could not delete the file: " + file.getAbsolutePath()));
				}
			}
		}
		return worked;
	}

	/**
	 * Copy a {@link File} The renameTo method does not allow action across NFS mounted filesystems this method is the
	 * workaround
	 * 
	 * @param fromFile
	 *            The existing File
	 * @param toFile
	 *            The new File
	 * @param checksum
	 *            if true, then a MD5 checksum is made to validate copying
	 * @return <b>true</b> if and only if the renaming succeeded; <b>false</b> otherwise
	 */
	public final static boolean copy(File fromFile, File toFile, boolean checksum) {

		BufferedInputStream inBuffer = null;
		BufferedOutputStream outBuffer = null;
		try {

			inBuffer = new BufferedInputStream(new FileInputStream(fromFile));
			outBuffer = new BufferedOutputStream(new FileOutputStream(toFile));

			int theByte = 0;

			while ((theByte = inBuffer.read()) > -1) {
				outBuffer.write(theByte);
			}

			inBuffer.close();
			outBuffer.flush();
			outBuffer.close();

			if (checksum) {
				String fromFileMD5 = StringHelper.getHexString(FileHelper.hashFileMd5(fromFile));
				String toFileMD5 = StringHelper.getHexString(FileHelper.hashFileMd5(toFile));
				if (!fromFileMD5.equals(toFileMD5)) {
					FileHelper.logger.error("Copying failed, as MD5 sums are not equal: " + fromFileMD5 + " / "
							+ toFileMD5);
					toFile.delete();

					return false;
				}
			}

			// cleanup if files are not the same length
			if (fromFile.length() != toFile.length()) {
				FileHelper.logger.error("Copying failed, as new files are not the same length: " + fromFile.length()
						+ " / " + toFile.length());
				toFile.delete();

				return false;
			}

		} catch (Exception e) {

			FileHelper.logger.error(e.getMessage(), e);
			return false;

		} finally {

			if (inBuffer != null) {
				try {
					inBuffer.close();
				} catch (IOException e) {
					FileHelper.logger.error("Error closing BufferedInputStream" + e);
				}
			}

			if (outBuffer != null) {
				try {
					outBuffer.close();
				} catch (IOException e) {
					FileHelper.logger.error("Error closing BufferedOutputStream" + e);
				}
			}
		}

		return true;
	}

	/**
	 * Move a File The renameTo method does not allow action across NFS mounted filesystems this method is the
	 * workaround
	 * 
	 * @param fromFile
	 *            The existing File
	 * @param toFile
	 *            The new File
	 * @return <b>true</b> if and only if the renaming succeeded; <b>false</b> otherwise
	 */
	public final static boolean move(File fromFile, File toFile) {

		if (fromFile.renameTo(toFile)) {
			return true;
		}

		FileHelper.logger.warn("Simple File.renameTo failed, trying copy/delete...");

		// delete if copy was successful, otherwise move will fail
		if (FileHelper.copy(fromFile, toFile, true)) {
			FileHelper.logger.info("Deleting fromFile: " + fromFile.getAbsolutePath());
			return fromFile.delete();
		}

		return false;
	}

	/**
	 * Finds the common parent for the files in the given list
	 * 
	 * @param files
	 *            the files to find the common parent for
	 * 
	 * @return the {@link File} representing the common parent, or null if there is none
	 */
	public static File findCommonParent(List<File> files) {

		// be gentle with bad input data
		if (files.size() == 0)
			return null;
		if (files.size() == 1)
			return files.get(0).getParentFile();

		File commonParent = null;
		int commonParentDepth = -1;

		// find the common parent among all the files
		for (int i = 0; i < files.size() - 1; i++) {

			// get first file
			File file = files.get(i);

			// get list of parents for this file
			List<File> parents = new ArrayList<File>();
			File parent = file.getParentFile();
			while (parent != null) {
				parents.add(parent);
				parent = parent.getParentFile();
			}
			// reverse
			Collections.reverse(parents);

			// and now the same for the next file
			File fileNext = files.get(i + 1);
			List<File> parentsNext = new ArrayList<File>();
			File parentNext = fileNext.getParentFile();
			while (parentNext != null) {
				parentsNext.add(parentNext);
				parentNext = parentNext.getParentFile();
			}
			// reverse
			Collections.reverse(parentsNext);

			//logger.info("Files: " + file + " / " + fileNext);

			// now find the common parent
			File newCommonParent = null;
			int newCommonParentDepth = -1;
			for (int j = 0; j < (parents.size()); j++) {

				// don't overflow the size of the next list of parents
				if (j >= parentsNext.size())
					break;

				// if we once found a common parent, and our current depth is 
				// greater than the one for the common parent, then stop as 
				// there can't be a deeper parent
				if (commonParent != null && j > commonParentDepth)
					break;

				// get the next parents to compare
				File aParent = parents.get(j);
				File bParent = parentsNext.get(j);

				//logger.info("Comparing " + aParent + " |||| " + bParent);

				// if they parent are the same, then break, as we won't 
				// have another match
				if (!aParent.equals(bParent))
					break;

				// save the parent and the depth where we found the parent
				newCommonParent = aParent;
				newCommonParentDepth = j;
			}

			// if no common parent was found, then break as there won't be one
			if (commonParent == null && newCommonParent == null)
				break;

			// if there is no new common parent, then check the next file
			if (newCommonParent == null)
				continue;

			// store the common parent
			commonParent = newCommonParent;
			commonParentDepth = newCommonParentDepth;
			//logger.info("Temporary common parent: (" + commonParentDepth + ") " + commonParent);
		}

		//logger.info("Common parent: " + commonParent);
		return commonParent;
	}

	/**
	 * Returns the size of the file in a human readable form. Everything smaller than 1024 bytes is returned as x bytes,
	 * next is KB, then MB and then GB
	 * 
	 * @param file
	 *            the file for which the humanized size is to be returned
	 * 
	 * @return the humanized form of the files size
	 */
	public final static String humanizeFileSize(File file) {
		return FileHelper.humanizeFileSize(file.length());
	}

	/**
	 * Returns the size of the file in a human readable form. Everything smaller than 1024 bytes is returned as x bytes,
	 * next is KB, then MB and then GB
	 * 
	 * @param fileSize
	 *            the size of a file for which the humanized size is to be returned
	 * 
	 * @return the humanized form of the files size
	 */
	public final static String humanizeFileSize(long fileSize) {
		if (fileSize < 1024)
			return String.format("%d bytes", fileSize);

		if (fileSize < 1048576)
			return String.format("%.1f KB", (fileSize / 1024.0d));

		if (fileSize < 1073741824)
			return String.format("%.1f MB", (fileSize / 1048576.0d));

		return String.format("%.1f GB", (fileSize / 1073741824.0d));
	}

	/**
	 * Creates the MD5 hash of the given file, returning the hash as a byte array. Use
	 * {@link StringHelper#getHexString(byte[])} to create a HEX string of the bytes
	 * 
	 * @param file
	 *            the file to hash
	 * 
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileMd5(File file) {
		return FileHelper.hashFile(file, "MD5");
	}

	/**
	 * Creates the SHA1 hash of the given file, returning the hash as a byte array. Use
	 * {@link StringHelper#getHexString(byte[])} to create a HEX string of the bytes
	 * 
	 * @param file
	 *            the file to hash
	 * 
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileSha1(File file) {
		return FileHelper.hashFile(file, "SHA-1");
	}

	/**
	 * Creates the SHA256 hash of the given file, returning the hash as a byte array. Use
	 * {@link StringHelper#getHexString(byte[])} to create a HEX string of the bytes
	 * 
	 * @param file
	 *            the file to hash
	 * 
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileSha256(File file) {
		return FileHelper.hashFile(file, "SHA-256");
	}

	/**
	 * Creates the hash of the given file with the given algorithm, returning the hash as a byte array. Use
	 * {@link StringHelper#getHexString(byte[])} to create a HEX string of the bytes
	 * 
	 * @param file
	 *            the file to hash
	 * @param algorithm
	 *            the hashing algorithm to use
	 * 
	 * @return the hash as a byte array
	 */
	public static byte[] hashFile(File file, String algorithm) {
		try {
			InputStream fis = new FileInputStream(file);

			byte[] buffer = new byte[1024];
			MessageDigest complete = MessageDigest.getInstance(algorithm);
			int numRead;
			do {
				numRead = fis.read(buffer);
				if (numRead > 0) {
					complete.update(buffer, 0, numRead);
				}
			} while (numRead != -1);
			fis.close();

			return complete.digest();
		} catch (Exception e) {
			throw new RuntimeException("Something went wrong while hashing file: " + file.getAbsolutePath());
		}
	}

	/**
	 * Helper method to append bytes to a specified file. The file is created if it does not exist otherwise the bytes
	 * are simply appended
	 * 
	 * @param dstFile
	 *            the file to append to
	 * @param bytes
	 *            the bytes to append
	 */
	public static void appendFilePart(File dstFile, byte[] bytes) {
		FileOutputStream outputStream = null;
		try {

			outputStream = new FileOutputStream(dstFile, true);
			outputStream.write(bytes);
			outputStream.flush();

		} catch (IOException e) {
			throw new RuntimeException("Could not create and append the bytes to the file " + dstFile.getAbsolutePath());
		} finally {
			if (outputStream != null) {
				try {
					outputStream.close();
				} catch (IOException e) {
					FileHelper.logger.error("Exception while closing FileOutputStream " + e.getLocalizedMessage());
				}
			}
		}
	}
}
