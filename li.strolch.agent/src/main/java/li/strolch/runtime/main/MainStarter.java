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

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class MainStarter {

	private static final Logger logger = LoggerFactory.getLogger(MainStarter.class);

	private static final String OPT_ROOT_PATH = "root-path";
	private static final String OPT_ENV = "env";

	private Options options;
	private String env;
	private File pathF;

	private StrolchAgent agent;

	public MainStarter() {

		Options op = new Options();
		Option rootPathOption = new Option("p", OPT_ROOT_PATH, true, "root path to strolch runtime");
		rootPathOption.setOptionalArg(false);
		rootPathOption.setRequired(true);
		op.addOption(rootPathOption);

		Option envOption = new Option("e", OPT_ENV, true, "environment to load from configuration file");
		envOption.setOptionalArg(false);
		envOption.setRequired(true);
		op.addOption(envOption);

		this.options = op;
	}

	public int start(String[] args) {
		int ret = parseArgs(args);
		if (ret != 0)
			return ret;

		setup();
		initialize();
		start();

		return keepAlive();
	}

	public int parseArgs(String[] args) {

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

		this.env = line.getOptionValue(OPT_ENV);
		if (StringHelper.isEmpty(this.env)) {
			logger.error("env argument is missing!");
			printUsage();
			return 1;
		}

		String pathS = line.getOptionValue(OPT_ROOT_PATH);
		this.pathF = new File(pathS);
		if (!this.pathF.exists()) {
			logger.error(MessageFormat.format("Path parameter does not exist at: {0}", pathS));
			printUsage();
			return 1;
		}

		return 0;
	}

	public void setup() {
		this.agent = new StrolchBootstrapper().setupByRoot(this.env, this.pathF);
	}

	public void initialize() {
		getAgent().initialize();
	}

	public void start() {
		getAgent().start();
	}

	public void stop() {
		getAgent().stop();
	}

	public void destroy() {
		getAgent().destroy();
	}

	public int keepAlive() {

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
		logger.info("Strolch application " + getAgent().getApplicationName() + " started ");
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

	public StrolchAgent getAgent() {
		return this.agent;
	}
}
