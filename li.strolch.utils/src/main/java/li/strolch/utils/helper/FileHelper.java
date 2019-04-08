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
package li.strolch.utils.helper;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import li.strolch.utils.DataUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class for dealing with files
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class FileHelper {

	private static final int MAX_FILE_SIZE = 50 * 1024 * 1024;
	private static final Logger logger = LoggerFactory.getLogger(FileHelper.class);

	/**
	 * Reads the contents of a file into a byte array.
	 *
	 * @param file
	 * 		the file to read
	 *
	 * @return the contents of a file as a string
	 */
	public static final byte[] readFile(File file) {
		if (file.length() > MAX_FILE_SIZE)
			throw new RuntimeException(
					String.format("Only allowed to read files up to %s. File too large: %s", //$NON-NLS-1$
							humanizeFileSize(MAX_FILE_SIZE), humanizeFileSize(file.length())));

		byte[] data = new byte[(int) file.length()];
		int pos = 0;

		try (BufferedInputStream in = new BufferedInputStream(Files.newInputStream(file.toPath()))) {
			byte[] bytes = new byte[8192];
			int read;
			while ((read = in.read(bytes)) != -1) {
				System.arraycopy(bytes, 0, data, pos, read);
				pos += read;
			}
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Filed does not exist " + file.getAbsolutePath()); //$NON-NLS-1$
		} catch (IOException e) {
			throw new RuntimeException("Could not read file " + file.getAbsolutePath()); //$NON-NLS-1$
		}

		return data;
	}

	/**
	 * Reads the contents of a file into a string. Note, no encoding is checked. It is expected to be UTF-8
	 *
	 * @param file
	 * 		the file to read
	 *
	 * @return the contents of a file as a string
	 */
	public static final String readFileToString(File file) {

		try (BufferedReader bufferedReader = new BufferedReader(new FileReader(file));) {

			StringBuilder sb = new StringBuilder();

			String line;

			while ((line = bufferedReader.readLine()) != null) {
				sb.append(line + "\n"); //$NON-NLS-1$
			}

			return sb.toString();

		} catch (FileNotFoundException e) {
			throw new RuntimeException("File does not exist " + file.getAbsolutePath()); //$NON-NLS-1$
		} catch (IOException e) {
			throw new RuntimeException("Could not read file " + file.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/**
	 * Reads the contents of a {@link InputStream} into a string. Note, no encoding is checked. It is expected to be
	 * UTF-8
	 *
	 * @param stream
	 * 		the stream to read
	 *
	 * @return the contents of a file as a string
	 */
	public static final String readStreamToString(InputStream stream) {

		try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(stream));) {

			StringBuilder sb = new StringBuilder();

			String line;

			while ((line = bufferedReader.readLine()) != null) {
				sb.append(line + "\n"); //$NON-NLS-1$
			}

			return sb.toString();

		} catch (IOException e) {
			throw new RuntimeException("Could not read strean " + stream); //$NON-NLS-1$
		}
	}

	/**
	 * Writes the given byte array to the given file
	 *
	 * @param bytes
	 * 		the data to write to the file
	 * @param dstFile
	 * 		the path to which to write the data
	 */
	public static final void writeToFile(byte[] bytes, File dstFile) {

		try (BufferedOutputStream out = new BufferedOutputStream(Files.newOutputStream(dstFile.toPath()))) {
			out.write(bytes);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Filed does not exist " + dstFile.getAbsolutePath()); //$NON-NLS-1$
		} catch (IOException e) {
			throw new RuntimeException("Could not write to file " + dstFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/**
	 * Writes the string to dstFile
	 *
	 * @param string
	 * 		string to write to file
	 * @param dstFile
	 * 		the file to write to
	 */
	public static final void writeStringToFile(String string, File dstFile) {

		try (BufferedWriter bufferedwriter = new BufferedWriter(new FileWriter(dstFile));) {
			bufferedwriter.write(string);
		} catch (FileNotFoundException e) {
			throw new RuntimeException("Filed does not exist " + dstFile.getAbsolutePath()); //$NON-NLS-1$
		} catch (IOException e) {
			throw new RuntimeException("Could not write to file " + dstFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/**
	 * Deletes files recursively. No question asked, but logging is done in case of problems
	 *
	 * @param file
	 * 		the file to delete
	 * @param log
	 * 		true to enable logging
	 *
	 * @return true if all went well, and false if it did not work. The log will contain the problems encountered
	 */
	public final static boolean deleteFile(File file, boolean log) {
		return FileHelper.deleteFiles(new File[] { file }, log);
	}

	/**
	 * Deletes files recursively. No question asked, but logging is done in case of problems
	 *
	 * @param files
	 * 		the files to delete
	 * @param log
	 * 		true to enable logging
	 *
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
					FileHelper.logger.warn("Could not empty the directory: " + file.getAbsolutePath()); //$NON-NLS-1$
				} else {
					done = file.delete();
					if (done) {
						if (log)
							FileHelper.logger.info("Deleted DIR  " + file.getAbsolutePath()); //$NON-NLS-1$
					} else {
						worked = false;
						FileHelper.logger
								.warn("Could not delete the directory: " + file.getAbsolutePath()); //$NON-NLS-1$
					}
				}
			} else {
				boolean done = file.delete();
				if (done) {
					if (log)
						FileHelper.logger.info("Deleted FILE " + file.getAbsolutePath()); //$NON-NLS-1$
				} else {
					worked = false;
					FileHelper.logger.warn(("Could not delete the file: " + file.getAbsolutePath())); //$NON-NLS-1$
				}
			}
		}
		return worked;
	}

	/**
	 * <p>
	 * Copy a given list of {@link File Files}. Recursively copies the files and directories to the destination.
	 * </p>
	 *
	 * @param srcFiles
	 * 		The source files to copy
	 * @param dstDirectory
	 * 		The destination where to copy the files
	 * @param checksum
	 * 		if true, then a MD5 checksum is made to validate copying
	 *
	 * @return <b>true</b> if and only if the copying succeeded; <b>false</b> otherwise
	 */
	public final static boolean copy(File[] srcFiles, File dstDirectory, boolean checksum) {

		if (!dstDirectory.isDirectory() || !dstDirectory.canWrite()) {
			String msg = "Destination is not a directory or is not writeable: {0}"; //$NON-NLS-1$
			throw new IllegalArgumentException(MessageFormat.format(msg, dstDirectory.getAbsolutePath()));
		}

		for (File srcFile : srcFiles) {

			File dstFile = new File(dstDirectory, srcFile.getName());
			if (srcFile.isDirectory()) {
				dstFile.mkdir();
				if (!copy(srcFile.listFiles(), dstFile, checksum)) {
					String msg = "Failed to copy contents of {0} to {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, srcFile.getAbsolutePath(), dstFile.getAbsolutePath());
					logger.error(msg);
					return false;
				}
			} else {
				if (!copy(srcFile, dstFile, checksum)) {
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Copy a {@link File} The renameTo method does not allow action across NFS mounted filesystems this method is the
	 * workaround
	 *
	 * @param fromFile
	 * 		The existing File
	 * @param toFile
	 * 		The new File
	 * @param checksum
	 * 		if true, then a MD5 checksum is made to validate copying
	 *
	 * @return <b>true</b> if and only if the renaming succeeded; <b>false</b> otherwise
	 */
	public final static boolean copy(File fromFile, File toFile, boolean checksum) {

		try (BufferedInputStream inBuffer = new BufferedInputStream(Files.newInputStream(fromFile.toPath()));
				BufferedOutputStream outBuffer = new BufferedOutputStream(Files.newOutputStream(toFile.toPath()))) {

			int theByte = 0;

			while ((theByte = inBuffer.read()) > -1) {
				outBuffer.write(theByte);
			}

			outBuffer.flush();

			if (checksum) {
				String fromFileMD5 = StringHelper.toHexString(FileHelper.hashFileMd5(fromFile));
				String toFileMD5 = StringHelper.toHexString(FileHelper.hashFileMd5(toFile));
				if (!fromFileMD5.equals(toFileMD5)) {
					FileHelper.logger.error(MessageFormat
							.format("Copying failed, as MD5 sums are not equal: {0} / {1}", //$NON-NLS-1$
									fromFileMD5, toFileMD5));
					toFile.delete();

					return false;
				}
			}

			// cleanup if files are not the same length
			if (fromFile.length() != toFile.length()) {
				String msg = "Copying failed, as new files are not the same length: {0} / {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, fromFile.length(), toFile.length());
				FileHelper.logger.error(msg);
				toFile.delete();

				return false;
			}

		} catch (Exception e) {
			String msg = MessageFormat
					.format("Failed to copy path from {0} to + {1} due to:", fromFile, toFile); //$NON-NLS-1$
			FileHelper.logger.error(msg, e);
			return false;
		}

		return true;
	}

	/**
	 * Move a File The renameTo method does not allow action across NFS mounted filesystems this method is the
	 * workaround
	 *
	 * @param fromFile
	 * 		The existing File
	 * @param toFile
	 * 		The new File
	 *
	 * @return <b>true</b> if and only if the renaming succeeded; <b>false</b> otherwise
	 */
	public final static boolean move(File fromFile, File toFile) {

		if (fromFile.renameTo(toFile)) {
			return true;
		}

		FileHelper.logger.warn("Simple File.renameTo failed, trying copy/delete..."); //$NON-NLS-1$

		// delete if copy was successful, otherwise move will fail
		if (FileHelper.copy(fromFile, toFile, true)) {
			FileHelper.logger.info("Deleting fromFile: " + fromFile.getAbsolutePath()); //$NON-NLS-1$
			return fromFile.delete();
		}

		return false;
	}

	/**
	 * Finds the common parent for the files in the given list
	 *
	 * @param files
	 * 		the files to find the common parent for
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
			List<File> parents = new ArrayList<>();
			File parent = file.getParentFile();
			while (parent != null) {
				parents.add(parent);
				parent = parent.getParentFile();
			}
			// reverse
			Collections.reverse(parents);

			// and now the same for the next file
			File fileNext = files.get(i + 1);
			List<File> parentsNext = new ArrayList<>();
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
	 * 		the file for which the humanized size is to be returned
	 *
	 * @return the humanized form of the files size
	 */
	public static String humanizeFileSize(File file) {
		return humanizeFileSize(file.length());
	}

	/**
	 * Returns the size of the file in a human readable form. Everything smaller than 1024 bytes is returned as x bytes,
	 * next is KB, then MB and then GB
	 *
	 * @param fileSize
	 * 		the size of a file for which the humanized size is to be returned
	 *
	 * @return the humanized form of the files size
	 */
	@SuppressWarnings("nls")
	public static String humanizeFileSize(long fileSize) {
		return DataUnit.Bytes.humanizeBytesValue(fileSize);
	}

	/**
	 * Creates the MD5 hash of the given file, returning the hash as a byte array. Use {@link
	 * StringHelper#toHexString(byte[])} to create a HEX string of the bytes
	 *
	 * @param file
	 * 		the file to hash
	 *
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileMd5(File file) {
		return FileHelper.hashFile(file, "MD5"); //$NON-NLS-1$
	}

	/**
	 * Creates the SHA1 hash of the given file, returning the hash as a byte array. Use {@link
	 * StringHelper#toHexString(byte[])} to create a HEX string of the bytes
	 *
	 * @param file
	 * 		the file to hash
	 *
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileSha1(File file) {
		return FileHelper.hashFile(file, "SHA-1"); //$NON-NLS-1$
	}

	/**
	 * Creates the SHA256 hash of the given file, returning the hash as a byte array. Use {@link
	 * StringHelper#toHexString(byte[])} to create a HEX string of the bytes
	 *
	 * @param file
	 * 		the file to hash
	 *
	 * @return the hash as a byte array
	 */
	public static byte[] hashFileSha256(File file) {
		return FileHelper.hashFile(file, "SHA-256"); //$NON-NLS-1$
	}

	/**
	 * Creates the hash of the given file with the given algorithm, returning the hash as a byte array. Use {@link
	 * StringHelper#toHexString(byte[])} to create a HEX string of the bytes
	 *
	 * @param file
	 * 		the file to hash
	 * @param algorithm
	 * 		the hashing algorithm to use
	 *
	 * @return the hash as a byte array
	 */
	public static byte[] hashFile(File file, String algorithm) {
		try (InputStream fis = Files.newInputStream(file.toPath())) {

			byte[] buffer = new byte[1024];
			MessageDigest complete = MessageDigest.getInstance(algorithm);
			int numRead;
			do {
				numRead = fis.read(buffer);
				if (numRead > 0) {
					complete.update(buffer, 0, numRead);
				}
			} while (numRead != -1);

			return complete.digest();
		} catch (Exception e) {
			throw new RuntimeException(
					"Something went wrong while hashing file: " + file.getAbsolutePath()); //$NON-NLS-1$
		}
	}

	/**
	 * Helper method to append bytes to a specified file. The file is created if it does not exist otherwise the bytes
	 * are simply appended
	 *
	 * @param dstFile
	 * 		the file to append to
	 * @param bytes
	 * 		the bytes to append
	 */
	public static void appendFilePart(File dstFile, byte[] bytes) {

		try (OutputStream outputStream = Files.newOutputStream(dstFile.toPath(), StandardOpenOption.APPEND)) {

			outputStream.write(bytes);
			outputStream.flush();

		} catch (IOException e) {
			throw new RuntimeException(
					"Could not create and append the bytes to the file " + dstFile.getAbsolutePath()); //$NON-NLS-1$
		}
	}
}
