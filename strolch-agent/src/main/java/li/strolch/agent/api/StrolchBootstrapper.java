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
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Properties;

import static java.util.Objects.requireNonNull;

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
	public static final String ENV_STROLCH_ENV = "STROLCH_ENV";
	public static final String ENV_STROLCH_PATH = "STROLCH_PATH";

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

	public void setEnvironmentOverride(String environmentOverride) {
		this.environmentOverride = environmentOverride;
	}

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

	public StrolchAgent setupByRoot(String environment, File rootPath) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("rootPath must be set!", rootPath);
		this.environment = environment;

		this.configPathF = new File(rootPath, PATH_CONFIG);
		this.dataPathF = new File(rootPath, PATH_DATA);
		this.tempPathF = new File(rootPath, PATH_TEMP);

		return setup();
	}

	public StrolchAgent setupByCopyingRoot(String environment, File rootSrcPath, File rootDstPath) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("rootPath must be set!", rootSrcPath);
		DBC.PRE.assertNotNull("rootPath must be set!", rootDstPath);

		this.environment = environment;

		// root path: readable directory
		if (!rootSrcPath.isDirectory() || !rootSrcPath.canRead()) {
			String msg = "[{0}] Root src path is not readable at {1}";
			msg = MessageFormat.format(msg, environment, rootSrcPath);
			throw new StrolchConfigurationException(msg);
		}

		// Make sure config exists in this root src
		File configPathF = new File(rootSrcPath, PATH_CONFIG);
		File configurationFile = new File(configPathF, ConfigurationParser.STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "[{0}] Source Configuration file is not readable at {1}";
			msg = MessageFormat.format(msg, environment, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// if destination exists, make sure it is a directory and empty
		if (rootDstPath.exists()) {
			if (!rootDstPath.isDirectory()) {
				String msg = "[{0}] Destination root exists and is not a directory at {1}";
				msg = MessageFormat.format(msg, environment, rootDstPath.getAbsolutePath());
				throw new StrolchConfigurationException(msg);
			}
			if (requireNonNull(rootDstPath.list()).length != 0) {
				String msg = "[{0}] Destination root exists and is not empty at {1}";
				msg = MessageFormat.format(msg, environment, rootDstPath.getAbsolutePath());
				throw new StrolchConfigurationException(msg);
			}
		} else if (!rootDstPath.mkdir()) {
			String msg
					= "[{0}] Destination root does not exist and could not be created. Either parent does not exist, or permission is denied at {1}";
			msg = MessageFormat.format(msg, environment, rootDstPath.getAbsolutePath());
			throw new StrolchConfigurationException(msg);
		}

		logger.info("[{}] Copying source {} to {}", environment, rootSrcPath.getAbsolutePath(),
				rootDstPath.getAbsolutePath());

		if (!FileHelper.copy(rootSrcPath.listFiles(), rootDstPath, true)) {
			throw new RuntimeException(
					MessageFormat.format("[{0}] Failed to copy source files from {1} to {2}", environment,
							rootSrcPath.getAbsolutePath(), rootDstPath.getAbsolutePath()));
		}

		this.configPathF = new File(rootDstPath, PATH_CONFIG);
		this.dataPathF = new File(rootDstPath, PATH_DATA);
		this.tempPathF = new File(rootDstPath, PATH_TEMP);

		return setup();
	}

	/**
	 * Set up Strolch by evaluating the environment from {@link StrolchEnvironment#getEnvironmentFromResourceEnv(Class)}
	 * and then delegating to {@link #setupByBootstrapFile(String, File)}
	 *
	 * @param clazz         the class from which to load the resource as stream
	 * @param bootstrapFile the bootstrap file to load
	 *
	 * @return the Agent which is setup
	 */
	public StrolchAgent setupByBootstrapFile(Class<?> clazz, File bootstrapFile) {
		DBC.PRE.assertNotNull("clazz must be set!", clazz);
		DBC.PRE.assertNotNull("bootstrapFile must be set!", bootstrapFile);
		this.environment = StrolchEnvironment.getEnvironmentFromResourceEnv(clazz);
		parseBoostrapFile(bootstrapFile);
		return setup();
	}

	/**
	 * Set up Strolch by evaluating the environment from {@link StrolchEnvironment#getEnvironmentFromResourceEnv(Class)}
	 * and then delegating to {@link #setupByBootstrapFile(String, File)}
	 *
	 * @param clazz         the class from which to load the resource as stream
	 * @param bootstrapFile the input stream to the bootstrap file to load
	 *
	 * @return the Agent which is setup
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
	 * Set up Strolch by loading the given bootstrap file for configuration
	 *
	 * @param environment   the environment to load from the boostrap file
	 * @param bootstrapFile the input stream to the bootstrap file to load
	 *
	 * @return the Agent which is setup
	 */
	public StrolchAgent setupByBootstrapFile(String environment, InputStream bootstrapFile) {
		DBC.PRE.assertNotEmpty("Environment must be set!", environment);
		DBC.PRE.assertNotNull("bootstrapFile must be set!", bootstrapFile);
		this.environment = environment;
		parseBoostrapFile(bootstrapFile);
		return setup();
	}

	private StrolchAgent setup() {

		DBC.PRE.assertNotEmpty("Environment must be set!", this.environment);
		DBC.PRE.assertNotNull("configPathF must be set!", this.configPathF);
		DBC.PRE.assertNotNull("dataPathF must be set!", this.dataPathF);
		DBC.PRE.assertNotNull("tempPathF must be set!", this.tempPathF);

		// config path: readable directory
		if (!this.configPathF.isDirectory() || !this.configPathF.canRead()) {
			String msg = "[{0}] Config path is not readable at {1}";
			msg = MessageFormat.format(msg, environment, this.configPathF);
			throw new StrolchConfigurationException(msg);
		}
		// configuration file must exist
		// get path to configuration file
		File configurationFile = new File(this.configPathF, ConfigurationParser.STROLCH_CONFIGURATION_XML);
		if (!configurationFile.isFile() || !configurationFile.canRead()) {
			String msg = "[{0}] Configuration file is not readable at {1}";
			msg = MessageFormat.format(msg, environment, configurationFile);
			throw new StrolchConfigurationException(msg);
		}

		// data path: writable directory
		if (!this.dataPathF.exists() && !this.dataPathF.mkdir()) {
			String msg = "[{0}] Could not create missing data path at {1}";
			msg = MessageFormat.format(msg, environment, this.dataPathF);
			throw new StrolchConfigurationException(msg);
		}
		if (!this.dataPathF.isDirectory() || !this.dataPathF.canRead() || !this.dataPathF.canWrite()) {
			String msg = "[{0}] Data path is not a directory or readable or writeable at {1}";
			msg = MessageFormat.format(msg, environment, this.dataPathF);
			throw new StrolchConfigurationException(msg);
		}

		// tmp path: writable directory
		if (!this.tempPathF.exists() && !this.tempPathF.mkdir()) {
			String msg = "[{0}] Could not create missing temp path at {1}";
			msg = MessageFormat.format(msg, environment, this.tempPathF);
			throw new StrolchConfigurationException(msg);
		}
		if (!this.tempPathF.isDirectory() || !this.tempPathF.canRead() || !this.tempPathF.canWrite()) {
			String msg = "[{0}] Temp path is not a directory or readable or writeable at {1}";
			msg = MessageFormat.format(msg, environment, this.tempPathF);
			throw new StrolchConfigurationException(msg);
		}

		LoggingLoader.reloadLogging(this.configPathF);

		String env;
		if (StringHelper.isEmpty(this.environmentOverride)) {
			env = this.environment;
		} else {
			logger.info("[{}] Environment override to ''{}''", this.environment, this.environmentOverride);
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
			throw new StrolchConfigurationException("Environment "
					+ this.environment
					+ " not configured in bootstrap configuration "
					+ bootstrapFile.getAbsolutePath());
		}

		evaluatePaths();
	}

	private void parseBoostrapFile(InputStream bootstrapStream) {

		// parse the document using ourselves as the DefaultHandler
		XmlHelper.parseDocument(bootstrapStream, this);

		if (!this.envFound) {
			throw new StrolchConfigurationException("Environment "
					+ this.environment
					+ " not configured in bootstrap configuration from given stream!");
		}

		evaluatePaths();
	}

	private void evaluatePaths() {

		// validate the parsed data
		if (!this.defaultAllowed) {
			if (StringHelper.isEmpty(this.configS) || StringHelper.isEmpty(this.dataS) || StringHelper.isEmpty(
					this.tempS)) {
				String msg = "One element of "
						+ Arrays.toString(new String[]{CONFIG, DATA, TEMP})
						+ " is not set and environment "
						+ this.environment
						+ " does not have attribute "
						+ DEFAULT
						+ "=\"true\". Either set the value or allow using default values!";
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

		switch (qName) {
			case STROLCH_BOOTSTRAP:
				break;

			case ENV:
				if (attributes.getValue(ID).equals(this.environment)) {
					this.insideEnv = true;
					this.envFound = true;
				} else {
					this.insideEnv = false;
				}

				String defaultS = attributes.getValue(DEFAULT);
				this.defaultAllowed = defaultS != null && StringHelper.parseBoolean(defaultS);

				break;

			case ENVIRONMENT:
			case ROOT:
			case CONFIG:
			case DATA:
			case TEMP:

				if (this.insideEnv)
					this.textB = new StringBuilder();

				break;

			default:
				throw new StrolchConfigurationException("Unhandled element " + qName);
		}
	}

	@Override
	public void endElement(String uri, String localName, String qName) {

		switch (qName) {
			case STROLCH_BOOTSTRAP:
				break;

			case ENV:
				this.insideEnv = false;
				break;

			case ENVIRONMENT:
				if (this.insideEnv)
					this.environmentOverride = this.textB.toString();
				break;
			case ROOT:
				if (this.insideEnv)
					this.rootS = this.textB.toString();
				break;
			case CONFIG:
				if (this.insideEnv)
					this.configS = this.textB.toString();
				break;
			case DATA:
				if (this.insideEnv)
					this.dataS = this.textB.toString();
				break;
			case TEMP:
				if (this.insideEnv)
					this.tempS = this.textB.toString();
				break;

			default:
				throw new StrolchConfigurationException("Unhandled element " + qName);
		}

		this.textB = null;
	}

	@Override
	public void characters(char[] ch, int start, int length) {
		if (this.textB != null)
			this.textB.append(ch, start, length);
	}
}
