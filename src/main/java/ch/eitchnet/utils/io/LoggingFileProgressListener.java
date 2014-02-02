package ch.eitchnet.utils.io;

import java.text.MessageFormat;

import org.slf4j.Logger;

import ch.eitchnet.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class LoggingFileProgressListener implements FileProgressListener {

	private final Logger logger;
	private final String name;

	/**
	 * @param logger
	 * @param name
	 */
	public LoggingFileProgressListener(Logger logger, String name) {
		this.logger = logger;
		this.name = name;
	}

	@Override
	public void begin(long size) {
		String msg = "Starting to read {0} with a size of {1}"; //$NON-NLS-1$
		this.logger.info(MessageFormat.format(msg, this.name, FileHelper.humanizeFileSize(size)));
	}

	@Override
	public void progress(int percent, long position) {
		String msg = "Read {0}% of {1} at position {2}"; //$NON-NLS-1$
		this.logger.info(MessageFormat.format(msg, percent, this.name, FileHelper.humanizeFileSize(position)));
	}

	@Override
	public void end(int percent, long position) {
		String msg = "Finished reading {0} at position {1} ({2}%)"; //$NON-NLS-1$
		this.logger.info(MessageFormat.format(msg, this.name, percent, FileHelper.humanizeFileSize(position)));
	}
}
