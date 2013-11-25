/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.main;

import java.io.File;
import java.text.MessageFormat;
import java.util.concurrent.atomic.AtomicBoolean;

import li.strolch.runtime.agent.StrolchAgent;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class MainStarter {

	private static final Logger logger = LoggerFactory.getLogger(MainStarter.class);
	private Options options;
	private StrolchAgent agent;

	public MainStarter() {
		Options op = new Options();
		Option rootPathOption = new Option("p", "root-path", true, "root path to strolch runtime");
		rootPathOption.setOptionalArg(false);
		op.addOption(rootPathOption);
		this.options = op;
	}

	public int start(String[] args) {

		// create the parser
		CommandLineParser parser = new GnuParser();
		CommandLine line;
		try {
			// parse the command line arguments
			line = parser.parse(this.options, args);
		} catch (ParseException exp) {
			// oops, something went wrong
			logger.error("Parsing failed.  Reason: " + exp.getMessage());
			printUsage();
			return 1;
		}

		String pathS = line.getOptionValue("root-path");
		File pathF = new File(pathS);
		if (!pathF.exists()) {
			logger.info(MessageFormat.format("Path parameter does not exist at: {0}", pathS));
			printUsage();
			return 1;
		}

		logger.info("Starting Agent...");
		this.setAgent(new StrolchAgent());
		this.getAgent().setup(pathF);
		this.getAgent().initialize();
		this.getAgent().start();

		final AtomicBoolean atomicBoolean = new AtomicBoolean();

		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				synchronized (MainStarter.class) {
					System.out.println("VM Shutting down. Stopping Strolch...");
					System.out.println("");
					System.out.println("Strolch application " + MainStarter.this.getAgent().getApplicationName()
							+ " shutting down...");
					MainStarter.this.getAgent().stop();
					MainStarter.this.getAgent().destroy();
					System.out.println("Strolch application " + MainStarter.this.getAgent().getApplicationName()
							+ " shut down. Exiting VM.");

					atomicBoolean.set(true);
					MainStarter.class.notify();
				}
			}
		});

		logger.info("");
		logger.info("Strolch application " + this.getAgent().getApplicationName() + " started ");
		while (!atomicBoolean.get()) {
			synchronized (MainStarter.class) {
				try {
					MainStarter.class.wait();
				} catch (InterruptedException e) {
					logger.error("Interrupted.");
					return 1;
				}
			}
		}
		return 0;
	}

	private void printUsage() {

		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("java -classpath lib/ -jar <jar.containing.main.class>.jar <options>", this.options);
	}

	/**
	 * @return the agent
	 */
	public StrolchAgent getAgent() {
		return this.agent;
	}

	/**
	 * @param agent
	 *            the agent to set
	 */
	public void setAgent(StrolchAgent agent) {
		this.agent = agent;
	}
}
