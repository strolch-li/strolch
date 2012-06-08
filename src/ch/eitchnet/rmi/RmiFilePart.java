package ch.eitchnet.rmi;

import java.io.Serializable;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RmiFilePart implements Serializable {

	//
	private static final long serialVersionUID = 1L;

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
	 *            the name of the file being referenced. This is either just the name, or a path relative to the type
	 * @param fileType
	 *            defines the type of file being uploaded or retrieved. This defines in which path the file resides
	 */
	public RmiFilePart(String fileName, String fileType) {

		if (fileName == null || fileName.isEmpty())
			throw new RuntimeException("fileName may not be empty!");
		if (fileType == null || fileType.isEmpty())
			throw new RuntimeException("fileType may not be empty!");

		this.fileName = fileName;
		this.fileType = fileType;

		this.partOffset = 0;
		this.partLength = RmiFileHandler.MAX_PART_SIZE;
		this.partBytes = null;
	}

	/**
	 * @return the fileLength
	 */
	public long getFileLength() {
		return fileLength;
	}

	/**
	 * @param fileLength
	 *            the fileLength to set
	 */
	public void setFileLength(long fileLength) {
		this.fileLength = fileLength;
	}

	/**
	 * @return the fileHash
	 */
	public String getFileHash() {
		return fileHash;
	}

	/**
	 * @param fileHash
	 *            the fileHash to set
	 */
	public void setFileHash(String fileHash) {
		this.fileHash = fileHash;
	}

	/**
	 * @return the fileType
	 */
	public String getFileType() {
		return fileType;
	}

	/**
	 * @return the partOffset
	 */
	public long getPartOffset() {
		return partOffset;
	}

	/**
	 * @param partOffset
	 *            the partOffset to set
	 */
	public void setPartOffset(long partOffset) {
		this.partOffset = partOffset;
	}

	/**
	 * @return the partLength
	 */
	public int getPartLength() {
		return partLength;
	}

	/**
	 * @param partLength
	 *            the partLength to set
	 */
	public void setPartLength(int partLength) {
		this.partLength = partLength;
	}

	/**
	 * @return the partBytes
	 */
	public byte[] getPartBytes() {
		return partBytes;
	}

	/**
	 * @param partBytes
	 *            the partBytes to set
	 */
	public void setPartBytes(byte[] partBytes) {
		this.partBytes = partBytes;
	}

	/**
	 * @return the lastPart
	 */
	public boolean isLastPart() {
		return lastPart;
	}

	/**
	 * @param lastPart
	 *            the lastPart to set
	 */
	public void setLastPart(boolean lastPart) {
		this.lastPart = lastPart;
	}

	/**
	 * @return the fileName
	 */
	public String getFileName() {
		return fileName;
	}
}
