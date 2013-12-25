/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.runtime.main;

import java.io.File;
import java.text.MessageFormat;
import java.util.concurrent.atomic.AtomicBoolean;

import li.strolch.runtime.agent.api.StrolchAgent;

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
