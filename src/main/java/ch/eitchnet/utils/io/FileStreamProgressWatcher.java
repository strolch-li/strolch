package ch.eitchnet.utils.io;

import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * File stream progress monitoring thread
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FileStreamProgressWatcher implements Runnable {

	private static final Logger logger = LoggerFactory.getLogger(FileStreamProgressWatcher.class);
	private ProgressableFileInputStream inputStream;
	private boolean run = false;
	private FileProgressListener progressListener;
	private long millis;

	/**
	 * @param millis
	 * @param progressListener
	 * @param inputStream
	 */
	public FileStreamProgressWatcher(long millis, FileProgressListener progressListener,
			ProgressableFileInputStream inputStream) {
		this.millis = millis;
		this.progressListener = progressListener;
		this.inputStream = inputStream;
	}

	@Override
	public void run() {
		this.run = true;

		this.progressListener.begin(this.inputStream.getFileSize());

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

				if (this.run) {
					Thread.sleep(this.millis);
				}

			} catch (InterruptedException e) {

				logger.info(MessageFormat.format("Work stopped: {0}", e.getLocalizedMessage())); //$NON-NLS-1$
				this.run = false;
				int percentComplete = this.inputStream.getPercentComplete();
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