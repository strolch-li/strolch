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
package li.strolch.agent.api;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.agent.impl.ComponentContainerImpl;
import li.strolch.exception.StrolchException;
import li.strolch.model.Locator;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.utils.ExecutorPool;
import li.strolch.utils.concurrent.ElementLockingHandler;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.SystemHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.management.*;
import java.text.MessageFormat;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static li.strolch.model.Tags.Json.*;
import static li.strolch.runtime.configuration.ConfigurationParser.parseConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchAgent {

	public static final String AGENT_VERSION_PROPERTIES = "/agentVersion.properties";

	public static final String PROP_TRY_LOCK_TIME_UNIT = "tryLockTimeUnit";
	public static final String PROP_TRY_LOCK_TIME = "tryLockTime";

	private static final Logger logger = LoggerFactory.getLogger(StrolchAgent.class);

	private final StrolchVersion appVersion;

	private ComponentContainerImpl container;
	private StrolchConfiguration strolchConfiguration;

	private ExecutorPool executorPool;
	private ElementLockingHandler<Locator> lockHandler;

	private JsonObject systemState;
	private long systemStateUpdateTime;

	private VersionQueryResult versionQueryResult;

	public StrolchAgent(StrolchVersion appVersion) {
		this.appVersion = appVersion;
	}

	public StrolchConfiguration getStrolchConfiguration() {
		return this.strolchConfiguration;
	}

	public RuntimeConfiguration getRuntimeConfiguration() {
		return this.strolchConfiguration.getRuntimeConfiguration();
	}

	public ComponentContainer getContainer() {
		return this.container;
	}

	/**
	 * @see ComponentContainer#hasComponent(Class)
	 */
	public boolean hasComponent(Class<?> clazz) {
		return this.container.hasComponent(clazz);
	}

	/**
	 * @see ComponentContainer#getComponent(Class)
	 */
	public <T> T getComponent(Class<T> clazz) throws IllegalArgumentException {
		return this.container.getComponent(clazz);
	}

	/**
	 * @see ComponentContainer#getComponentByName(String)
	 */
	public <T extends StrolchComponent> T getComponentByName(String name) {
		return getContainer().getComponentByName(name);
	}

	/**
	 * @see ComponentContainer#getComponentsOrderedByRoot()
	 */
	public List<StrolchComponent> getComponentsOrderedByRoot() {
		return this.container.getComponentsOrderedByRoot();
	}

	public PrivilegeHandler getPrivilegeHandler() throws IllegalArgumentException {
		return this.container.getPrivilegeHandler();
	}

	/**
	 * @see ComponentContainer#getRealm(String)
	 */
	public StrolchRealm getRealm(String realm) throws StrolchException {
		return getComponent(RealmHandler.class).getRealm(realm);
	}

	/**
	 * @see ComponentContainer#getRealm(Certificate)
	 */
	public StrolchRealm getRealm(Certificate certificate) throws StrolchException {
		return this.container.getRealm(certificate);
	}

	/**
	 * @see StrolchRealm#openTx(Certificate, Class, boolean)
	 */
	public StrolchTransaction openTx(Certificate certificate, Class<?> clazz, boolean readOnly) {
		return this.container.getRealm(certificate).openTx(certificate, clazz, readOnly);
	}

	/**
	 * @see StrolchRealm#openTx(Certificate, String, boolean)
	 */
	public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
		return this.container.getRealm(certificate).openTx(certificate, action, readOnly);
	}

	/**
	 * @see PrivilegeHandler#runAs(String, PrivilegedRunnable)
	 */
	public void runAs(String systemUser, PrivilegedRunnable runnable) throws Exception {
		getPrivilegeHandler().runAs(systemUser, runnable);
	}

	/**
	 * @see PrivilegeHandler#runAsAgent(PrivilegedRunnable)
	 */
	public void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		getPrivilegeHandler().runAsAgent(runnable);
	}

	/**
	 * @see PrivilegeHandler#runAsAgentWithResult(PrivilegedRunnableWithResult)
	 */
	public <T> T runAsWithResult(String systemUser, PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return getPrivilegeHandler().runAsWithResult(systemUser, runnable);
	}

	/**
	 * @see PrivilegeHandler#runAsAgentWithResult(PrivilegedRunnableWithResult)
	 */
	public <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return getPrivilegeHandler().runAsAgentWithResult(runnable);
	}

	/**
	 * @return the name of this application as is defined in the configuration
	 */
	public String getApplicationName() {
		return this.strolchConfiguration.getRuntimeConfiguration().getApplicationName();
	}

	/**
	 * @return the currently loaded environment of this application as is defined in the configuration
	 */
	public String getEnvironment() {
		return this.strolchConfiguration.getRuntimeConfiguration().getEnvironment();
	}

	/**
	 * @return the agent's {@link Locale}
	 */
	public Locale getLocale() {
		return this.strolchConfiguration.getRuntimeConfiguration().getLocale();
	}

	/**
	 * @return the agent's time zone
	 */
	public String getTimezone() {
		return this.strolchConfiguration.getRuntimeConfiguration().getTimezone();
	}

	/**
	 * Return the {@link ExecutorService} instantiated for this agent
	 *
	 * @return the {@link ExecutorService} instantiated for this agent
	 */
	public ExecutorService getExecutor() {
		return getExecutor("Agent");
	}

	public ExecutorService getExecutor(String poolName) {
		return this.executorPool.getExecutor(poolName);
	}

	/**
	 * Return the {@link ExecutorService} instantiated for this agent
	 *
	 * @return the {@link ExecutorService} instantiated for this agent
	 */
	public ExecutorService getSingleThreadExecutor() {
		return getSingleThreadExecutor("Agent");
	}

	public ExecutorService getSingleThreadExecutor(String poolName) {
		return this.executorPool.getSingleThreadExecutor(poolName);
	}

	/**
	 * Return the {@link ScheduledExecutorService} instantiated for this agent
	 *
	 * @return the {@link ScheduledExecutorService} instantiated for this agent
	 */
	public ScheduledExecutorService getScheduledExecutor() {
		return getScheduledExecutor("Agent");
	}

	public ScheduledExecutorService getScheduledExecutor(String poolName) {
		return this.executorPool.getScheduledExecutor(poolName);
	}

	public ElementLockingHandler<Locator> getLockHandler() {
		return this.lockHandler;
	}

	/**
	 * Initializes the underlying container and prepares the executor services. Before calling this method,
	 * {@link #setup(String, File, File, File)} must have ben called
	 */
	public void initialize() {
		if (this.container == null)
			throw new RuntimeException("Please call setup first!");

		this.executorPool = new ExecutorPool();

		RuntimeConfiguration configuration = this.strolchConfiguration.getRuntimeConfiguration();
		TimeUnit timeUnit = TimeUnit.valueOf(configuration.getString(PROP_TRY_LOCK_TIME_UNIT, TimeUnit.SECONDS.name()));
		long time = configuration.getLong(PROP_TRY_LOCK_TIME, 10L);
		this.lockHandler = new ElementLockingHandler<>(getScheduledExecutor(), timeUnit, time);
		this.container.initialize();
	}

	/**
	 * Starts the container
	 */
	public void start() {
		if (this.container == null)
			throw new RuntimeException("Please call setup first!");
		this.lockHandler.start();
		this.container.start();
	}

	/**
	 * Stops the container
	 */
	public void stop() {
		if (this.container != null)
			this.container.stop();
		if (this.lockHandler != null)
			this.lockHandler.stop();
	}

	/**
	 * Destroys the container and the executor services
	 */
	public void destroy() {

		if (this.executorPool != null)
			this.executorPool.destroy();

		if (this.container != null)
			this.container.destroy();
		this.container = null;
	}

	/**
	 * <p>
	 * <b>Note:</b> Use {@link StrolchBootstrapper} instead of calling this method directly!
	 * </p>
	 *
	 * <p>
	 * Sets up the agent by parsing the configuration file and initializes the given environment
	 * </p>
	 *
	 * @param environment the current environment
	 * @param configPathF the path to the config directory
	 * @param dataPathF   the path to the data directory
	 * @param tempPathF   the path to the temp directory
	 */
	void setup(String environment, File configPathF, File dataPathF, File tempPathF) {

		String msg = "[{0}] Setting up Strolch Container using the following paths:";
		logger.info(MessageFormat.format(msg, environment));
		logger.info(" - Config: {}", configPathF.getAbsolutePath());
		logger.info(" - Data: {}", dataPathF.getAbsolutePath());
		logger.info(" - Temp: {}", tempPathF.getAbsolutePath());
		logger.info(" - user.dir: {}", SystemHelper.getUserDir());

		this.strolchConfiguration = parseConfiguration(environment, configPathF, dataPathF, tempPathF);

		ComponentContainerImpl container = new ComponentContainerImpl(this);
		container.setup(this.strolchConfiguration);

		this.container = container;

		RuntimeConfiguration config = this.strolchConfiguration.getRuntimeConfiguration();
		logger.info(MessageFormat.format("Setup Agent {0}:{1}", config.getApplicationName(), config.getEnvironment()));
	}

	protected void assertContainerStarted() {
		if (this.container == null || this.container.getState() != ComponentState.STARTED) {
			String msg = "Container is not yet started!";
			throw new IllegalStateException(msg);
		}
	}

	/**
	 * @return Returns the pseudo unique Id to be used during object creation from external services.
	 */
	public static String getUniqueId() {
		return StringHelper.getUniqueId();
	}

	/**
	 * @return Returns the pseudo unique Id to be used during object creation from external services.
	 */
	public static Long getUniqueIdLong() {
		return StringHelper.getUniqueIdLong();
	}

	/**
	 * Returns the version of this agent
	 *
	 * @return the version of this agent
	 */
	public VersionQueryResult getVersion() {
		if (this.versionQueryResult == null) {

			VersionQueryResult queryResult = new VersionQueryResult();
			queryResult.setAppVersion(this.appVersion);

			Properties properties = new Properties();

			try (InputStream stream = getClass().getResourceAsStream(AGENT_VERSION_PROPERTIES)) {
				properties.load(stream);
				RuntimeConfiguration runtimeConfiguration = getStrolchConfiguration().getRuntimeConfiguration();
				AgentVersion agentVersion = new AgentVersion(runtimeConfiguration.getApplicationName(),
						runtimeConfiguration.getEnvironment(), runtimeConfiguration.getLocale(),
						runtimeConfiguration.getTimezone(), properties);
				queryResult.setAgentVersion(agentVersion);
			} catch (IOException e) {
				String msg = MessageFormat.format("Failed to read version properties for agent: {0}", e.getMessage());
				queryResult.getErrors().add(msg);
				logger.error(msg, e);
			}

			Set<Class<?>> componentTypes = this.container.getComponentTypes();
			for (Class<?> componentType : componentTypes) {
				StrolchComponent component = (StrolchComponent) this.container.getComponent(componentType);
				try {
					ComponentVersion componentVersion = component.getVersion();
					queryResult.add(componentVersion);
				} catch (Exception e) {
					String msg = "Failed to read version properties for component {0} due to: {1}";
					msg = MessageFormat.format(msg, component.getName(), e.getMessage());
					queryResult.getErrors().add(msg);
					logger.error(msg, e);
				}
			}

			this.versionQueryResult = queryResult;
		}

		return this.versionQueryResult;
	}

	public JsonObject getSystemState(long updateInterval, TimeUnit updateIntervalUnit) {

		if (this.systemState == null
				|| System.currentTimeMillis() - this.systemStateUpdateTime > updateIntervalUnit.toMillis(
				updateInterval)) {
			this.systemState = new JsonObject();

			JsonObject osJ = new JsonObject();
			this.systemState.add(OPERATING_SYSTEM, osJ);
			osJ.addProperty(OS_NAME, SystemHelper.osName);
			osJ.addProperty(OS_ARCH, SystemHelper.osArch);
			osJ.addProperty(OS_VERSION, SystemHelper.osVersion);
			osJ.addProperty(JAVA_VENDOR, SystemHelper.javaVendor);
			osJ.addProperty(JAVA_VERSION, SystemHelper.javaVersion);

			OperatingSystemMXBean osMXBean = ManagementFactory.getOperatingSystemMXBean();
			osJ.addProperty(AVAILABLE_PROCESSORS, osMXBean.getAvailableProcessors());
			osJ.addProperty(SYSTEM_LOAD_AVERAGE, osMXBean.getSystemLoadAverage());

			RuntimeMXBean runtimeMXBean = ManagementFactory.getRuntimeMXBean();
			osJ.addProperty(START_TIME, ISO8601.toString(runtimeMXBean.getStartTime()));
			osJ.addProperty(UPTIME, runtimeMXBean.getUptime());

			// memory
			JsonObject memoryJ = new JsonObject();
			this.systemState.add(MEMORY, memoryJ);

			if (osMXBean instanceof com.sun.management.OperatingSystemMXBean os) {
				memoryJ.addProperty(TOTAL_PHYSICAL_MEMORY_SIZE, os.getTotalMemorySize());
				memoryJ.addProperty(FREE_PHYSICAL_MEMORY_SIZE, os.getFreeMemorySize());
				memoryJ.addProperty(FREE_SWAP_SPACE_SIZE, os.getFreeSwapSpaceSize());
				memoryJ.addProperty(COMMITTED_VIRTUAL_MEMORY_SIZE, os.getCommittedVirtualMemorySize());
			}

			MemoryMXBean memoryMXBean = ManagementFactory.getMemoryMXBean();
			MemoryUsage heapMemoryUsage = memoryMXBean.getHeapMemoryUsage();
			memoryJ.addProperty(HEAP_MEMORY_USAGE_INIT, heapMemoryUsage.getInit());
			memoryJ.addProperty(HEAP_MEMORY_USAGE_USED, heapMemoryUsage.getUsed());
			memoryJ.addProperty(HEAP_MEMORY_USAGE_MAX, heapMemoryUsage.getMax());
			memoryJ.addProperty(HEAP_MEMORY_USAGE_COMMITTED, heapMemoryUsage.getCommitted());

			// disk space
			JsonArray rootsJ = new JsonArray();
			this.systemState.add(ROOTS, rootsJ);
			File[] roots = File.listRoots();
			for (File root : roots) {
				JsonObject rootJ = new JsonObject();
				rootsJ.add(rootJ);
				rootJ.addProperty(PATH, root.getAbsolutePath());
				rootJ.addProperty(USABLE_SPACE, root.getUsableSpace());
				rootJ.addProperty(USED_SPACE, root.getTotalSpace() - root.getFreeSpace());
				rootJ.addProperty(FREE_SPACE, root.getFreeSpace());
				rootJ.addProperty(TOTAL_SPACE, root.getTotalSpace());
			}

			this.systemStateUpdateTime = System.currentTimeMillis();
		}

		return this.systemState;
	}

	public void reloadStrolchConfiguration() {
		RuntimeConfiguration runtimeConfig = this.strolchConfiguration.getRuntimeConfiguration();
		File configPathF = runtimeConfig.getConfigPath();
		File dataPathF = runtimeConfig.getDataPath();
		File tempPathF = runtimeConfig.getTempPath();
		StrolchConfiguration newConfig = parseConfiguration(runtimeConfig.getEnvironment(), configPathF, dataPathF,
				tempPathF);

		for (String name : this.container.getComponentNames()) {
			ComponentConfiguration newComponentConfig = newConfig.getComponentConfiguration(name);
			StrolchComponent existingComponent = this.container.getComponentByName(name);
			ComponentConfiguration existingComponentConfiguration = existingComponent.getConfiguration();
			existingComponentConfiguration.updateProperties(newComponentConfig.getAsMap());
		}

		RuntimeConfiguration newRuntimeConfiguration = newConfig.getRuntimeConfiguration();
		runtimeConfig.updateProperties(newRuntimeConfiguration.getAsMap());
		runtimeConfig.setLocale(newRuntimeConfiguration.getLocale());
		runtimeConfig.setSupportedLanguages(newRuntimeConfiguration.getSupportedLanguages());
	}

	public JsonObject toJson() {
		JsonObject agentJ = getStrolchConfiguration().getRuntimeConfiguration().toJson();

		JsonArray componentsJ = new JsonArray();
		List<StrolchComponent> components = getComponentsOrderedByRoot();
		for (StrolchComponent component : components) {
			componentsJ.add(component.toJson());
		}
		agentJ.add(COMPONENTS, componentsJ);

		return agentJ;
	}
}
