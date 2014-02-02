package ch.eitchnet.utils.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * <p>
 * This sub class of {@link FileInputStream} allows to follow the currently read bytes of a {@link File}. In conjunction
 * with a {@link Thread} and a {@link FileProgressListener} it is possible to track the progress of a long running job on
 * bigger files
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ProgressableFileInputStream extends FileInputStream {

	private long fileSize;
	private long bytesRead;
	private boolean closed;

	/**
	 * Constructs a normal {@link FileInputStream} with the given {@link File}
	 * 
	 * @param file
	 *            the file to read
	 * @throws FileNotFoundException
	 *             thrown if the {@link File} does not exist
	 */
	public ProgressableFileInputStream(File file) throws FileNotFoundException {
		super(file);
		this.fileSize = file.length();
	}

	/**
	 * @see java.io.FileInputStream#read()
	 */
	@Override
	public int read() throws IOException {
		synchronized (this) {
			this.bytesRead++;
		}
		return super.read();
	}

	/**
	 * @see java.io.FileInputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		int read = super.read(b, off, len);
		if (read != -1) {
			synchronized (this) {
				this.bytesRead += read;
			}
		}
		return read;
	}

	/**
	 * @see java.io.FileInputStream#read(byte[])
	 */
	@Override
	public int read(byte[] b) throws IOException {
		int read = super.read(b);
		if (read != -1) {
			synchronized (this) {
				this.bytesRead += read;
			}
		}
		return read;
	}

	/**
	 * @see java.io.FileInputStream#skip(long)
	 */
	@Override
	public long skip(long n) throws IOException {
		long skip = super.skip(n);
		if (skip != -1) {
			synchronized (this) {
				this.bytesRead += skip;
			}
		}
		return skip;
	}

	/**
	 * @see java.io.FileInputStream#close()
	 */
	@Override
	public void close() throws IOException {
		this.closed = true;
		super.close();
	}

	/**
	 * Returns the size of the file being read
	 * 
	 * @return the size of the file being read
	 */
	public long getFileSize() {
		return this.fileSize;
	}

	/**
	 * Returns the number of bytes already read
	 * 
	 * @return the number of bytes already read
	 */
	public long getBytesRead() {
		synchronized (this) {
			if (this.bytesRead > this.fileSize)
				this.bytesRead = this.fileSize;
			return this.bytesRead;
		}
	}

	/**
	 * Returns the percent read of the file
	 * 
	 * @return the percentage complete of the process
	 */
	public int getPercentComplete() {

		long currentRead;
		synchronized (this) {
			if (this.bytesRead > this.fileSize)
				this.bytesRead = this.fileSize;
			currentRead = this.bytesRead;
		}

		double read = (100.0d / this.fileSize * currentRead);
		return (int) read;
	}

	/**
	 * Returns true if {@link #close()} was called, false otherwise
	 * 
	 * @return the closed
	 */
	public boolean isClosed() {
		return this.closed;
	}
}
