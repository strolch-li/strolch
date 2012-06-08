package ch.eitchnet.xmlpers;

import java.io.File;
import java.io.IOException;

import org.apache.log4j.Logger;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistencePathBuilder {
	private static final Logger logger = Logger.getLogger(XmlPersistencePathBuilder.class);

	/**
	 * 
	 */
	public static final String FILE_EXT = ".xml";

	/**
	 * 
	 */
	public static final int EXT_LENGTH = FILE_EXT.length();

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

		logger.info("Using base path " + basePath);
	}

	/**
	 * @param id
	 * @return
	 */
	public String getFilename(String id) {
		return id.concat(FILE_EXT);
	}

	/**
	 * @param filename
	 * @return
	 */
	public String getId(String filename) {
		if (filename.charAt(filename.length() - EXT_LENGTH) != '.')
			throw new XmlPersistenceExecption("The filename does not have a . at index "
					+ (filename.length() - EXT_LENGTH));

		return filename.substring(0, filename.length() - EXT_LENGTH);
	}

	/**
	 * @param type
	 * 
	 * @return
	 */
	public String getPath(String type) {

		StringBuilder sb = new StringBuilder(basePath);
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

		StringBuilder sb = new StringBuilder(basePath);
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

		StringBuilder sb = new StringBuilder(basePath);
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
