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
package li.strolch.agent.impl;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.*;
import static li.strolch.runtime.configuration.RuntimeConfiguration.PROP_TIMEZONE;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.*;

import li.strolch.agent.api.*;
import li.strolch.exception.StrolchException;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.runtime.privilege.PrivilegedRunnable;
import li.strolch.runtime.privilege.PrivilegedRunnableWithResult;
import li.strolch.utils.helper.SystemHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComponentContainerImpl implements ComponentContainer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerImpl.class);

	private final StrolchAgent agent;
	private Map<Class<?>, StrolchComponent> componentMap;
	private Map<String, ComponentController> controllerMap;
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private ComponentState state;

	private ComponentContainerStateHandler containerStateHandler;

	public ComponentContainerImpl(StrolchAgent agent) {
		this.agent = agent;
		this.state = ComponentState.UNDEFINED;
	}

	@Override
	public StrolchAgent getAgent() {
		return this.agent;
	}

	@Override
	public ComponentState getState() {
		return this.state;
	}

	@Override
	public Set<Class<?>> getComponentTypes() {
		return this.componentMap.keySet();
	}

	@Override
	public boolean hasComponent(Class<?> clazz) {
		return this.componentMap.containsKey(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T getComponent(Class<T> clazz) throws IllegalArgumentException {
		T component = (T) this.componentMap.get(clazz);
		if (component == null) {
			String msg = "The component does not exist for class {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, clazz.getName());
			throw new IllegalArgumentException(msg);
		}
		return component;
	}

	@Override
	public PrivilegeHandler getPrivilegeHandler() throws IllegalArgumentException {
		return getComponent(PrivilegeHandler.class);
	}

	@Override
	public Set<String> getRealmNames() {
		return getComponent(RealmHandler.class).getRealmNames();
	}

	@Override
	public StrolchRealm getRealm(String realm) throws StrolchException {
		return getComponent(RealmHandler.class).getRealm(realm);
	}

	@Override
	public StrolchRealm getRealm(Certificate certificate) throws StrolchException {

		String realmName = certificate.getRealm();
		if (isEmpty(realmName)) {
			if (getRealmNames().contains(DEFAULT_REALM)) {
				realmName = DEFAULT_REALM;
			} else {
				String msg = "The User {0} is missing the property {1} and the Realm {2} can not be used as it does not exist!";
				throw new StrolchException(
						MessageFormat.format(msg, certificate.getUsername(), PROP_REALM, DEFAULT_REALM));
			}
		}

		try {
			return getComponent(RealmHandler.class).getRealm(realmName);
		} catch (StrolchException e) {
			String msg = "The User {0} has property {1} with value={2}, but the Realm does not eixst, or is not accessible by this user!";
			throw new StrolchException(MessageFormat.format(msg, certificate.getUsername(), PROP_REALM, realmName), e);
		}
	}

	@Override
	public void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		getPrivilegeHandler().runAsAgent(runnable);
	}

	@Override
	public <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return getPrivilegeHandler().runAsAgentWithResult(runnable);
	}

	private void setupComponent(Map<Class<?>, StrolchComponent> componentMap,
			Map<String, ComponentController> controllerMap, ComponentConfiguration componentConfiguration) {

		String componentName = componentConfiguration.getName();
		if (isEmpty(componentName))
			throw new IllegalStateException(
					"name missing for a component in env " + componentConfiguration.getRuntimeConfiguration()
							.getEnvironment());

		try {
			String api = componentConfiguration.getApi();
			String impl = componentConfiguration.getImpl();

			if (isEmpty(api))
				throw new IllegalStateException("api must be set for " + componentName);
			if (isEmpty(impl))
				throw new IllegalStateException("impl must be set for " + componentName);

			Class<?> apiClass = Class.forName(api);
			Class<?> implClass = Class.forName(impl);

			if (!apiClass.isAssignableFrom(implClass)) {
				String msg = "Component {0} has invalid configuration: Impl class {1} is not assignable to Api class {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, componentName, impl, api);
				throw new StrolchConfigurationException(msg);
			}

			if (!StrolchComponent.class.isAssignableFrom(implClass)) {
				String msg = "Component {0} has invalid configuration: Impl class {1} is not a subclass of {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, componentName, impl, StrolchComponent.class.getName());
				throw new StrolchConfigurationException(msg);
			}

			@SuppressWarnings("unchecked")
			Class<StrolchComponent> strolchComponentClass = (Class<StrolchComponent>) implClass;
			Constructor<StrolchComponent> constructor = strolchComponentClass.getConstructor(ComponentContainer.class,
					String.class);
			StrolchComponent strolchComponent = constructor.newInstance(this, componentName);
			strolchComponent.setup(componentConfiguration);

			componentMap.put(apiClass, strolchComponent);
			controllerMap.put(componentName, new ComponentController(strolchComponent));

		} catch (NoSuchMethodException e) {

			String msg = "Could not load class for component {0} due to missing constructor with signature (ComponentContainer.class, String.class)"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, componentName, e.getMessage());
			throw new StrolchConfigurationException(msg, e);

		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | SecurityException | IllegalArgumentException | InvocationTargetException e) {

			String msg = "Could not load class for component {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, componentName, e.getMessage());
			throw new StrolchConfigurationException(msg, e);
		}
	}

	public void setup(StrolchConfiguration strolchConfiguration) {
		this.state.validateStateChange(ComponentState.SETUP, "agent");

		long start = System.nanoTime();

		// set the application locale
		Locale.setDefault(strolchConfiguration.getRuntimeConfiguration().getLocale());
		String msg = "Application {0}:{1} is using locale {2} and timezone {3}"; //$NON-NLS-1$
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		System.setProperty("user.timezone", getTimezone());
		logger.info(MessageFormat.format(msg, applicationName, environment, Locale.getDefault(),
				System.getProperty("user.timezone")));

		// set up the container itself
		Map<Class<?>, StrolchComponent> componentMap = new HashMap<>();
		Map<String, ComponentController> controllerMap = new HashMap<>();
		Set<String> componentNames = strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration.getComponentConfiguration(
					componentName);

			// setup each component
			setupComponent(componentMap, controllerMap, componentConfiguration);
		}

		// then analyze dependencies
		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, controllerMap);
		this.dependencyAnalyzer.setupDependencies();

		// now save references
		this.componentMap = componentMap;
		this.controllerMap = controllerMap;

		// and configure the state handler
		this.containerStateHandler = new ComponentContainerStateHandler(this.dependencyAnalyzer, strolchConfiguration);

		this.state = ComponentState.SETUP;

		long took = System.nanoTime() - start;
		msg = "{0}:{1} Strolch Container setup with {2} components. Took {3}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, applicationName, environment, this.componentMap.size(),
				formatNanoDuration(took)));
	}

	public void initialize() {
		this.state.validateStateChange(ComponentState.INITIALIZED, "agent");

		long start = System.nanoTime();

		// now we can initialize the components
		String msg = "{0}:{1} Initializing {2} Strolch Components..."; //$NON-NLS-1$
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		containerStateHandler.initialize(rootUpstreamComponents);

		this.state = ComponentState.INITIALIZED;

		long took = System.nanoTime() - start;
		msg = "{0}:{1} All {2} Strolch Components have been initialized. Took {3}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
				formatNanoDuration(took)));
	}

	public void start() {
		this.state.validateStateChange(ComponentState.STARTED, "agent");

		long start = System.nanoTime();

		String msg = "{0}:{1} Starting {2} Strolch Components..."; //$NON-NLS-1$
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		containerStateHandler.start(rootUpstreamComponents);

		this.state = ComponentState.STARTED;

		logger.info("Garbage collecting after startup...");
		System.gc();
		logger.info(MessageFormat.format("System: {0}", SystemHelper.asString()));
		logger.info(MessageFormat.format("Memory: {0}", SystemHelper.getMemorySummary()));
		logger.info(MessageFormat.format("Using locale {0} with timezone {1}",
				getAgent().getStrolchConfiguration().getRuntimeConfiguration().getLocale().toLanguageTag(),
				getTimezone()));
		logger.info(
				MessageFormat.format("file.encoding: {0} / sun.jnu.encoding {1}", System.getProperty("file.encoding"),
						System.getProperty("sun.jnu.encoding")));

		String tookS = formatNanoDuration(System.nanoTime() - start);

		if (hasComponent(OperationsLog.class)) {
			for (String realmName : getRealmNames()) {
				getComponent(OperationsLog.class).addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()), LogSeverity.Info,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"), "agent.started") //
						.value("applicationName", applicationName) //
						.value("environment", environment) //
						.value("components", "" + this.controllerMap.size()) //
						.value("took", tookS));
			}
		}

		msg = "{0}:{1} All {2} Strolch Components started. Took {3}. Strolch is now ready to be used. Have fun =))"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(), tookS));
	}

	public void stop() {
		this.state.validateStateChange(ComponentState.STOPPED, "agent");

		long start = System.nanoTime();

		String environment = getEnvironment();
		String applicationName = getApplicationName();

		if (hasComponent(OperationsLog.class)) {
			for (String realmName : getRealmNames()) {
				getComponent(OperationsLog.class).addMessage(new LogMessage(realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()), LogSeverity.Info,
						LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"), "agent.stopping") //
						.value("applicationName", applicationName) //
						.value("environment", environment) //
						.value("components", "" + this.controllerMap.size()));
			}
		}

		String msg = "{0}:{1} Stopping {2} Strolch Components..."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to stop"); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.stop(rootUpstreamComponents);

			long took = System.nanoTime() - start;
			msg = "{0}:{1} All {2} Strolch Components have been stopped. Took {3}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
					formatNanoDuration(took)));
		}

		this.state = ComponentState.STOPPED;
	}

	public void destroy() {
		this.state.validateStateChange(ComponentState.DESTROYED, "agent");

		long start = System.nanoTime();

		String msg = "{0}:{1} Destroying {2} Strolch Components..."; //$NON-NLS-1$
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to destroy"); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.destroy(rootUpstreamComponents);

			long took = System.nanoTime() - start;
			msg = "{0}:{1} All {2} Strolch Components have been destroyed! Took {3}"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
					formatNanoDuration(took)));
			this.controllerMap.clear();
			this.componentMap.clear();
		}

		this.state = ComponentState.DESTROYED;

		this.controllerMap = null;
		this.componentMap = null;
	}

	private String getApplicationName() {
		return getAgent().getApplicationName();
	}

	private String getEnvironment() {
		return getAgent().getStrolchConfiguration().getRuntimeConfiguration().getEnvironment();
	}

	private String getTimezone() {
		return getAgent().getStrolchConfiguration().getRuntimeConfiguration()
				.getString(PROP_TIMEZONE, System.getProperty("user.timezone"));
	}
}
