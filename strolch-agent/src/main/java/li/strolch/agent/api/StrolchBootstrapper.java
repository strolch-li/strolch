/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.api;

import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.configuration.StrolchEnvironment;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.XmlHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Optional;
import java.util.Properties;

import static java.text.MessageFormat.format;
import static java.util.Objects.requireNonNull;
import static li.strolch.utils.helper.StringHelper.isEmpty;

/**
 * The {@code StrolchBootstrapper} class is responsible for bootstrapping and configuring a StrolchAgent instance. It
 * provides various methods to set up the Strolch environment, parse bootstrap files, and load configurations based on
 * the provided parameters such as environment, class, or bootstrap files.
 * <p>
 * This class extends {@link DefaultHandler} to handle XML parsing during bootstrap file processing.
 * <p>
 * The {@code StrolchBootstrapper} can load configurations from: - Environment variables - Bootstrap files -
 * User-defined directory structures - Root paths or copied configurations
 */
public class StrolchBootstrapper extends DefaultHandler {

	private static final Logger logger = LoggerFactory.getLogger(StrolchBootstrapper.class);

	public static final String APP_VERSION_PROPERTIES = "/appVersion.properties";

	private static final String SYS_PROP_USER_DIR = "user.dir";
	private static final String STROLCH_BOOTSTRAP = "StrolchBootstrap";
	private static final String ENV = "env";

	private static final String ID = "id";
	private static final String DEFAULT = "default";

	private static final String ENVIRONMENT = "environment";
	private static final String ROOT = "root";
	private static final String CONFIG = "config";
	private static final String DATA = "data";
	private static final String TEMP = "temp";

	public static final String FILE_BOOTSTRAP = "StrolchBootstrap.xml";
	public static final String ENV_STROLCH_ENVIRONMENT = "STROLCH_ENVIRONMENT";
	public static final String ENV_STROLCH_RUNTIME_PATH = "STROLCH_RUNTIME_PATH";

	public static final String PATH_CONFIG = "config";
	public static final String PATH_DATA = "data";
	public static final String PATH_TEMP = "temp";

	// input
	private String environment;

	// intermediary
	private boolean defaultAllowed;
	private String environmentOverride;
	private boolean envFound;
	private boolean insideEnv;
	private StringBuilder textB;

	private String rootS;
	private String configS;
	private String dataS;
	private String tempS;

	// result
	private File configPathF;
	private File dataPathF;
	private File tempPathF;

	private final StrolchVersion appVersion;

	/**
	 * <p>
	 * Bootstrap Strolch using the given app {@link StrolchVersion}. This version is used for information on the code
	 * base from which the agent is instantiated.
	 * </p>
	 *
	 * @param appVersion the app's version
	 */
	public StrolchBootstrapper(StrolchVersion appVersion) {
		DBC.PRE.assertNotNull("appVersion must be set!", appVersion);
		this.appVersion = appVersion;
	}

	/**
	 * <p>
	 * Bootstrap Strolch using the given {@link Class} from which to get the {@link #APP_VERSION_PROPERTIES} resource
	 * stream. The version is used for information on the code base from which the agent is instantiated.
	 * </p>
	 *
	 * @param appClass the class where the {@link #APP_VERSION_PROPERTIES} resource resides
	 */
	public StrolchBootstrapper(Class<?> appClass) {
		DBC.PRE.assertNotNull("appClass must be set!", appClass);

		Properties env = new Properties();
		try (InputStream in = appClass.getResourceAsStream(APP_VERSION_PROPERTIES)) {
			env.load(in);
		} catch (Exception e) {
			throw new IllegalArgumentException(
					"Could not find resource " + APP_VERSION_PROPERTIES + " on ClassLoader of class " + appClass);
		}

		this.appVersion = new StrolchVersion(env);
	}

	/**
	 * Sets up the {@link StrolchAgent} instance based on the user directory using the specified environment and
	 * sub-directory path. Paths for configuration, data, and temporary files will be derived and initialized relative
	 * to the given sub-directory path.
	 *
	 * @param environment the environment that the {@link StrolchAgent} should use; must not be empty
	 * @param subPath     the sub-directory path relative to the user directory; must not be empty
	 *
	 * @return the configured {@link StrolchAgent} instance
	 */
	public StrolchAgent setupByUserDir(String environment, String subPath) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotEmpty("Sub Path must be set!", subPath);
		this.environment = environment;

		File rootPathF = new File(System.getProperty(SYS_PROP_USER_DIR), subPath);
		this.configPathF = new File(rootPathF, PATH_CONFIG);
		this.dataPathF = new File(rootPathF, PATH_DATA);
		this.tempPathF = new File(rootPathF, PATH_TEMP);

		return setup();
	}

	/**
	 * Attempts to initialize and set up a {@link StrolchAgent} based on environment variables. Checks if the specified
	 * environment variables for the environment and runtime path are set and valid. If the conditions are met, the
	 * agent is initialized with these parameters.
	 *
	 * @param appClass the application {@link Class}, used to reference resources and configurations required for setup
	 *
	 * @return an {@link Optional} containing the initialized {@link StrolchAgent} if successful; an empty
	 * {@link Optional} if the environment variables are missing or invalid
	 *
	 * @throws IllegalStateException if the runtime path specified by the environment variable does not exist or is not
	 *                               a directory
	 */
	public Optional<StrolchAgent> trySetupByEnvironment(Class<?> appClass) {
		String sysEnv = System.getenv(ENV_STROLCH_ENVIRONMENT);
		String sysEnvPath = System.getenv(ENV_STROLCH_RUNTIME_PATH);
		if (isEmpty(sysEnv))
			return Optional.empty();

		if (isEmpty(sysEnvPath)) {
			logger.error("Detected environment var {}={} but environment var {} is not set!", ENV_STROLCH_ENVIRONMENT,
					sysEnv, ENV_STROLCH_RUNTIME_PATH);
			return Optional.empty();
		}

		logger.info("Detected environment var {}={} and {}={}", ENV_STROLCH_ENVIRONMENT, sysEnv,
				ENV_STROLCH_RUNTIME_PATH, sysEnvPath);

		File rootPath = new File(sysEnvPath);
		if (!rootPath.isDirectory())
			throw new IllegalStateException(format("Detected environment var {0}={1} but path {2}={3} does not exist!",
					ENV_STROLCH_RUNTIME_PATH, sysEnv, ENV_STROLCH_RUNTIME_PATH, sysEnvPath));

		StrolchBootstrapper bootstrapper = new StrolchBootstrapper(appClass);
		return Optional.of(bootstrapper.setupByRoot(sysEnv, rootPath));
	}

	/**
	 * Configures and initializes a {@link StrolchAgent} instance based on the specified environment and root directory.
	 * The method sets up paths for configuration, data, and temporary files relative to the provided root directory,
	 * then completes the setup process by invoking the {@code setup()} method.
	 *
	 * @param environment the environment identifier to be used for setup; must not be empty
	 * @param rootPath    the root directory containing the required configuration, data, and temporary paths; must not
	 *                    be null
	 *
	 * @return the configured {@link StrolchAgent} instance
	 */
	public StrolchAgent setupByRoot(String environment, File rootPath) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("rootPath must be set!", rootPath);
		this.environment = environment;

		this.configPathF = new File(rootPath, PATH_CONFIG);
		this.dataPathF = new File(rootPath, PATH_DATA);
		this.tempPathF = new File(rootPath, PATH_TEMP);

		return setup();
	}

	/**
	 * Sets up the StrolchAgent by loading configuration from the default bootstrap file.
	 *
	 * @param clazz the class used as a reference to locate the bootstrap file within the resource environment
	 *
	 * @return the configured StrolchAgent instance
	 */
	public StrolchAgent setupByBootstrapFile(Class<?> clazz) {
		logger.info("Setting up agent using bootstrap file...");
		String bootstrapFileName = "/" + FILE_BOOTSTRAP;
		InputStream bootstrapFile = clazz.getResourceAsStream(bootstrapFileName);
		if (bootstrapFile == null)
			throw new IllegalStateException("Bootstrap file " + FILE_BOOTSTRAP + " not found!");
		StrolchBootstrapper bootstrapper = new StrolchBootstrapper(clazz);
		return bootstrapper.setupByBootstrapFile(clazz, bootstrapFile);
	}

	/**
	 * Sets up the StrolchAgent by loading the specified bootstrap file for configuration.
	 *
	 * @param clazz         the class used to determine the resource environment
	 * @param bootstrapFile the input stream of the bootstrap file to be parsed
	 *
	 * @return the configured StrolchAgent instance
	 */
	public StrolchAgent setupByBootstrapFile(Class<?> clazz, InputStream bootstrapFile) {
		DBC.PRE.assertNotNull("clazz must be set!", clazz);
		DBC.PRE.assertNotNull("bootstrapFile must be set!", bootstrapFile);
		this.environment = StrolchEnvironment.getEnvironmentFromResourceEnv(clazz);
		parseBoostrapFile(bootstrapFile);
		return setup();
	}

	/**
	 * Set up Strolch by loading the given bootstrap file for configuration
	 *
	 * @param environment   the environment to load from the boostrap file
	 * @param bootstrapFile the bootstrap file to load
	 *
	 * @return the Agent which is setup
	 */
	public StrolchAgent setupByBootstrapFile(String environment, File bootstrapFile) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("bootstrapFile must be set!", bootstrapFile);
		this.environment = environment;
		parseBoostrapFile(bootstrapFile);
		return setup();
	}

	/**
	 * Configures and initializes a {@link StrolchAgent} instance by copying the contents of a source root directory to
	 * a destination root directory. The method ensures the validity and integrity of both source and destination
	 * directories, copies the necessary files, and sets up paths for configuration, data, and temporary files.
	 *
	 * @param environment the environment identifier for this setup process; must not be empty
	 * @param rootSrcPath the source root directory, which must exist, be a readable directory, and contain the required
	 *                    configuration file
	 * @param rootDstPath the destination root directory, which must either not exist (and be created) or be an empty
	 *                    directory if it already exists
	 *
	 * @return the configured {@link StrolchAgent} instance
	 *
	 * @throws IllegalArgumentException      if the environment is empty
	 * @throws NullPointerException          if the source or destination root paths are null
	 * @throws StrolchConfigurationException if the source root path is not a readable directory, if it lacks the
	 *                                       required configuration file, or if the destination root path is invalid
	 * @throws RuntimeException              if copying the source files to the destination directory fails
	 */
	public StrolchAgent setupByCopyingRoot(String environment, File rootSrcPath, File rootDstPath) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("rootPath must be set!", rootSrcPath);
		DBC.PRE.assertNotNull("rootPath must be set!", rootDstPath);

		this.environment = environment;

		// root path: readable directory
		if (!rootSrcPath.isDirectory() || !rootSrcPath.canRead()) {
			String msg = "[{0}] Root src path is not readable at {1}";
			msg = format(msg, environment, rootSrcPath);
			throw new StrolchConfigurationException(msg);
		}

		// Make sure config exists in this root src
		File configPathF = new File(rootSrcPath, PATH_CONFIG);
		File configurationFile = new File(configPathF, ConfigurationParser.STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "[{0}] Source Configuration file is not readable at {1}";
			msg = format(msg, environment, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// if destination exists, make sure it is a directory and empty
		if (rootDstPath.exists()) {
			if (!rootDstPath.isDirectory()) {
				String msg = "[{0}] Destination root exists and is not a directory at {1}";
				msg = format(msg, environment, rootDstPath.getAbsolutePath());
				throw new StrolchConfigurationException(msg);
			}
			if (requireNonNull(rootDstPath.list()).length != 0) {
				String msg = "[{0}] Destination root exists and is not empty at {1}";
				msg = format(msg, environment, rootDstPath.getAbsolutePath());
				throw new StrolchConfigurationException(msg);
			}
		} else if (!rootDstPath.mkdir()) {
			String msg
					= "[{0}] Destination root does not exist and could not be created. Either parent does not exist, or permission is denied at {1}";
			msg = format(msg, environment, rootDstPath.getAbsolutePath());
			throw new StrolchConfigurationException(msg);
		}

		logger.info("[{}] Copying source {} to {}", environment, rootSrcPath.getAbsolutePath(),
				rootDstPath.getAbsolutePath());

		if (!FileHelper.copy(rootSrcPath.listFiles(), rootDstPath, true)) {
			throw new RuntimeException(format("[{0}] Failed to copy source files from {1} to {2}", environment,
					rootSrcPath.getAbsolutePath(), rootDstPath.getAbsolutePath()));
		}

		this.configPathF = new File(rootDstPath, PATH_CONFIG);
		this.dataPathF = new File(rootDstPath, PATH_DATA);
		this.tempPathF = new File(rootDstPath, PATH_TEMP);

		return setup();
	}

	/**
	 * Sets up and initializes a StrolchAgent instance with the provided environment, configuration path, data path, and
	 * temporary path. Validates the paths, ensuring they meet the necessary requirements (existence, readability,
	 * writability, etc.). Throws a StrolchConfigurationException if any validation checks fail. Reloads logging
	 * configurations before initializing the agent.
	 *
	 * @return an instance of {@code StrolchAgent} configured for the specified environment
	 *
	 * @throws IllegalArgumentException      if required fields are not set
	 * @throws StrolchConfigurationException if any path validation checks fail
	 */
	private StrolchAgent setup() {

		DBC.PRE.assertNotEmpty("Environment must be set!", this.environment);
		DBC.PRE.assertNotNull("configPathF must be set!", this.configPathF);
		DBC.PRE.assertNotNull("dataPathF must be set!", this.dataPathF);
		DBC.PRE.assertNotNull("tempPathF must be set!", this.tempPathF);

		// config path: readable directory
		if (!this.configPathF.isDirectory() || !this.configPathF.canRead()) {
			String msg = "[{0}] Config path is not readable at {1}";
			msg = format(msg, environment, this.configPathF.getAbsolutePath());
			throw new StrolchConfigurationException(msg);
		}
		// configuration file must exist
		// get path to configuration file
		File configurationFile = new File(this.configPathF, ConfigurationParser.STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "[{0}] Configuration file is not readable at {1}";
			msg = format(msg, environment, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// data path: writable directory
		if (!this.dataPathF.exists() && !this.dataPathF.mkdir()) {
			String msg = "[{0}] Could not create missing data path at {1}";
			msg = format(msg, environment, this.dataPathF);
			throw new StrolchConfigurationException(msg);
		}
		if (!this.dataPathF.isDirectory() || !this.dataPathF.canRead() || !this.dataPathF.canWrite()) {
			String msg = "[{0}] Data path is not a directory or readable or writeable at {1}";
			msg = format(msg, environment, this.dataPathF);
			throw new StrolchConfigurationException(msg);
		}

		// tmp path: writable directory
		if (!this.tempPathF.exists() && !this.tempPathF.mkdir()) {
			String msg = "[{0}] Could not create missing temp path at {1}";
			msg = format(msg, environment, this.tempPathF);
			throw new StrolchConfigurationException(msg);
		}
		if (!this.tempPathF.isDirectory() || !this.tempPathF.canRead() || !this.tempPathF.canWrite()) {
			String msg = "[{0}] Temp path is not a directory or readable or writeable at {1}";
			msg = format(msg, environment, this.tempPathF);
			throw new StrolchConfigurationException(msg);
		}

		LoggingLoader.reloadLogging(this.configPathF);

		String env;
		if (StringHelper.isEmpty(this.environmentOverride)) {
			env = this.environment;
		} else {
			logger.info("[{}] Environment override to '{}'", this.environment, this.environmentOverride);
			env = this.environmentOverride;
		}

		StrolchAgent agent = new StrolchAgent(this.appVersion);
		agent.setup(env, this.configPathF, this.dataPathF, this.tempPathF);
		return agent;
	}

	private void parseBoostrapFile(File bootstrapFile) {
		// parse the document using ourselves as the DefaultHandler
		XmlHelper.parseDocument(bootstrapFile, this);

		if (!this.envFound) {
			throw new StrolchConfigurationException(
					format("Environment {0} not configured in bootstrap configuration {1}", this.environment,
							bootstrapFile.getAbsolutePath()));
		}

		evaluatePaths();
	}

	private void parseBoostrapFile(InputStream bootstrapStream) {

		// parse the document using ourselves as the DefaultHandler
		XmlHelper.parseDocument(bootstrapStream, this);

		if (!this.envFound) {
			throw new StrolchConfigurationException(
					format("Environment {0} not configured in bootstrap configuration from given stream!",
							this.environment));
		}

		evaluatePaths();
	}

	private void evaluatePaths() {

		// validate the parsed data
		if (!this.defaultAllowed) {
			if (StringHelper.isEmpty(this.configS) || StringHelper.isEmpty(this.dataS) || StringHelper.isEmpty(
					this.tempS)) {
				String msg = format(
						"One element of {0} is not set and environment {1} does not have attribute {2}=\"true\". Either set the value or allow using default values!",
						Arrays.toString(new String[]{CONFIG, DATA, TEMP}), this.environment, DEFAULT);
				throw new StrolchConfigurationException(msg);
			}
		}

		String root = StringHelper.isEmpty(this.rootS) ?
				new File(System.getProperty(SYS_PROP_USER_DIR)).getAbsolutePath() : this.rootS;
		String config = StringHelper.isEmpty(this.configS) ? PATH_CONFIG : this.configS;
		String data = StringHelper.isEmpty(this.dataS) ? PATH_DATA : this.dataS;
		String temp = StringHelper.isEmpty(this.tempS) ? PATH_TEMP : this.tempS;

		File rootPathF = new File(root);

		File tmp;

		tmp = new File(config);
		this.configPathF = tmp.isAbsolute() ? tmp : new File(rootPathF, config);

		tmp = new File(data);
		this.dataPathF = tmp.isAbsolute() ? tmp : new File(rootPathF, data);

		tmp = new File(temp);
		this.tempPathF = tmp.isAbsolute() ? tmp : new File(rootPathF, temp);
	}

	@Override
	public void startElement(String uri, String localName, String qName, Attributes attributes) {

		switch (localName) {
			case STROLCH_BOOTSTRAP -> {
			}
			case ENV -> {
				if (attributes.getValue(ID).equals(this.environment)) {
					this.insideEnv = true;
					this.envFound = true;
				} else {
					this.insideEnv = false;
				}

				String defaultS = attributes.getValue(DEFAULT);
				this.defaultAllowed = defaultS != null && StringHelper.parseBoolean(defaultS);
			}
			case ENVIRONMENT, ROOT, CONFIG, DATA, TEMP -> {

				if (this.insideEnv)
					this.textB = new StringBuilder();
			}
			default -> throw new StrolchConfigurationException("Unhandled element " + localName);
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) {

		switch (localName) {
			case STROLCH_BOOTSTRAP -> {
			}
			case ENV -> this.insideEnv = false;
			case ENVIRONMENT -> {
				if (this.insideEnv)
					this.environmentOverride = this.textB.toString();
			}
			case ROOT -> {
				if (this.insideEnv)
					this.rootS = this.textB.toString();
			}
			case CONFIG -> {
				if (this.insideEnv)
					this.configS = this.textB.toString();
			}
			case DATA -> {
				if (this.insideEnv)
					this.dataS = this.textB.toString();
			}
			case TEMP -> {
				if (this.insideEnv)
					this.tempS = this.textB.toString();
			}
			default -> throw new StrolchConfigurationException("Unhandled element " + localName);
		}

		this.textB = null;
	}

	@Override
	public void characters(char[] ch, int start, int length) {
		if (this.textB != null)
			this.textB.append(ch, start, length);
	}
}
