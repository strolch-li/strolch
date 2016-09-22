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

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * File stream progress monitoring thread
 * 
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class FileStreamProgressWatcher implements Runnable {

	private static final Logger logger = LoggerFactory.getLogger(FileStreamProgressWatcher.class);
	private ProgressableFileInputStream inputStream;
	private boolean run = false;
	private FileProgressListener progressListener;
	private long millis;

	public FileStreamProgressWatcher(long millis, FileProgressListener progressListener,
			ProgressableFileInputStream inputStream) {
		this.millis = millis;
		this.progressListener = progressListener;
		this.inputStream = inputStream;
	}

	@Override
	public void run() {
		this.run = true;
		this.progressListener.begin(this.inputStream.getPercentComplete(), this.inputStream.getBytesRead(),
				this.inputStream.getFileSize());

		while (this.run) {
			try {
				int percentComplete = this.inputStream.getPercentComplete();
				if (this.inputStream.isClosed()) {
					logger.info(MessageFormat.format("Input Stream is closed at: {0}%", percentComplete)); //$NON-NLS-1$
					this.run = false;
					this.progressListener.end(percentComplete, this.inputStream.getBytesRead());
				} else if (percentComplete < 100) {
					this.progressListener.progress(percentComplete, this.inputStream.getBytesRead());
				} else if (percentComplete >= 100) {
					this.run = false;
					this.progressListener.end(percentComplete, this.inputStream.getBytesRead());
				}

				if (this.run)
					Thread.sleep(this.millis);

			} catch (InterruptedException e) {
				this.run = false;
				int percentComplete = this.inputStream.getPercentComplete();
				if (percentComplete != 100)
					logger.info(MessageFormat.format("Work stopped: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
				this.progressListener.end(percentComplete, this.inputStream.getBytesRead());
			} catch (Exception e) {
				logger.error(e.getMessage(), e);
				this.run = false;
				int percentComplete = this.inputStream.getPercentComplete();
				this.progressListener.end(percentComplete, Long.MAX_VALUE);
			}
		}
	}
}