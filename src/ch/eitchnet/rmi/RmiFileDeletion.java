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
