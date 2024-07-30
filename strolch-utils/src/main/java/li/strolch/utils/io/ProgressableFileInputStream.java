/*
 * Copyright 2014 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.utils.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * <p>
 * This sub class of {@link FileInputStream} allows to follow the currently read bytes of a {@link File}. In conjunction
 * with a {@link Thread} and a {@link FileProgressListener} it is possible to track the progress of a long running job
 * on bigger files
 * </p>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ProgressableFileInputStream extends FileInputStream {

	private final Object lock = new Object();

	private final long fileSize;
	private volatile long bytesRead;
	private volatile boolean closed;

	/**
	 * Constructs a normal {@link FileInputStream} with the given {@link File}
	 *
	 * @param file
	 * 		the file to read
	 *
	 * @throws FileNotFoundException
	 * 		thrown if the {@link File} does not exist
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
		synchronized (this.lock) {
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
			synchronized (this.lock) {
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
			synchronized (this.lock) {
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
		synchronized (this.lock) {
			this.bytesRead += skip;
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
		synchronized (this.lock) {
			return Math.min(this.bytesRead, this.fileSize);
		}
	}

	/**
	 * Returns the percent read of the file
	 *
	 * @return the percentage complete of the process
	 */
	public int getPercentComplete() {

		long currentRead;
		synchronized (this.lock) {
			currentRead = Math.min(this.bytesRead, this.fileSize);
		}

		double read = (100.0d / this.fileSize * currentRead);
		return (int) Math.ceil(read);
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
