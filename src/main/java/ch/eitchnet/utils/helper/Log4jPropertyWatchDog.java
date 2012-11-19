/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.helper;

import java.io.File;

import org.apache.log4j.LogManager;
import org.apache.log4j.PropertyConfigurator;
import org.apache.log4j.helpers.LogLog;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class Log4jPropertyWatchDog extends Thread {

	/**
	 * The default delay between every file modification check, set to 60 seconds.
	 */
	public static final long DEFAULT_DELAY = 60000;

	/**
	 * The name of the file to observe for changes.
	 */
	protected String filename;

	/**
	 * The delay to observe between every check. By default set {@link #DEFAULT_DELAY}.
	 */
	protected long delay = Log4jPropertyWatchDog.DEFAULT_DELAY;

	protected File file;
	protected long lastModif = 0;
	protected boolean warnedAlready = false;
	protected boolean interrupted = false;

	/**
	 * @param filename
	 */
	protected Log4jPropertyWatchDog(String filename) {
		super("FileWatchdog");
		this.filename = filename;
		this.file = new File(filename);
		setDaemon(true);
		checkAndConfigure();
	}

	/**
	 * Set the delay to observe between each check of the file changes.
	 */
	public void setDelay(long delay) {
		this.delay = delay;
	}

	/**
	 * 
	 */
	protected void checkAndConfigure() {
		boolean fileExists;
		try {
			fileExists = this.file.exists();
		} catch (SecurityException e) {
			LogLog.warn("Was not allowed to read check file existance, file:[" + this.filename + "].");
			this.interrupted = true; // there is no point in continuing
			return;
		}

		if (fileExists) {
			long l = this.file.lastModified(); // this can also throw a SecurityException
			if (l > this.lastModif) { // however, if we reached this point this
				this.lastModif = l; // is very unlikely.
				doOnChange();
				this.warnedAlready = false;
			}
		} else {
			if (!this.warnedAlready) {
				LogLog.debug("[" + this.filename + "] does not exist.");
				this.warnedAlready = true;
			}
		}
	}

	/**
	 * Call {@link PropertyConfigurator#configure(String)} with the <code>filename</code> to reconfigure log4j.
	 */
	public void doOnChange() {
		PropertyConfigurator propertyConfigurator = new PropertyConfigurator();
		propertyConfigurator.doConfigure(this.filename, LogManager.getLoggerRepository());
	}

	/**
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {
		while (!this.interrupted) {
			try {
				Thread.sleep(this.delay);
			} catch (InterruptedException e) {
				// no interruption expected
				this.interrupted = true;
			}
			checkAndConfigure();
		}
	}
}
