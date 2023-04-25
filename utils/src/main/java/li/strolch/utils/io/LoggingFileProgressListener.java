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

import li.strolch.utils.helper.FileHelper;
import org.slf4j.Logger;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class LoggingFileProgressListener implements FileProgressListener {

	private final Logger logger;
	private final String name;

	public LoggingFileProgressListener(Logger logger, String name) {
		this.logger = logger;
		this.name = name;
	}

	@Override
	public void begin(int percent, long position, long size) {
		String msg = "Starting to read {0} {1} of {2} ({3}%)";
		log(MessageFormat.format(msg, this.name, position, FileHelper.humanizeFileSize(size), percent));
	}

	@Override
	public void progress(int percent, long position) {
		String msg = "Read {0}% of {1} at position {2}";
		log(MessageFormat.format(msg, percent, this.name, FileHelper.humanizeFileSize(position)));
	}

	@Override
	public void end(int percent, long position) {
		String msg = "Finished reading {0} at position {1} ({2}%)";
		log(MessageFormat.format(msg, this.name, FileHelper.humanizeFileSize(position), percent));
	}

	private void log(String msg) {
		this.logger.info(msg);
	}

}
