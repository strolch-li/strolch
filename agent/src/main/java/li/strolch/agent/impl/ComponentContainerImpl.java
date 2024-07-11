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
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.SystemHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.*;

import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.*;
import static li.strolch.runtime.configuration.RuntimeConfiguration.PROP_TIMEZONE;
import static li.strolch.utils.helper.ExceptionHelper.getRootCauseMessage;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;
import static li.strolch.utils.helper.StringHelper.isEmpty;

public class ComponentContainerImpl implements ComponentContainer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerImpl.class);

	private final StrolchAgent agent;
	private MapOfLists<Class<?>, StrolchComponent> componentsByType;
	private Map<String, StrolchComponent> componentsByName;
	private Map<String, ComponentController> controllerMap;
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private ComponentState state;

	private ComponentContainerStateHandler containerStateHandler;
	private StrolchConfiguration strolchConfiguration;

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
		return this.componentsByType.keySet();
	}

	@Override
	public Set<String> getComponentNames() {
		return this.componentsByName.keySet();
	}

	@Override
	public boolean hasComponent(Class<?> clazz) {
		return this.componentsByType != null && this.componentsByType.containsList(clazz);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T getComponent(Class<T> clazz) throws IllegalArgumentException {
		List<StrolchComponent> components = this.componentsByType.getList(clazz);
		if (components == null || components.isEmpty()) {
			String msg = "No component exists for class {0}";
			msg = MessageFormat.format(msg, clazz.getName());
			throw new IllegalArgumentException(msg);
		}
		if (components.size() > 1) {
			String msg
					= "Component clazz {0} is ambiguous as there are {1} components registered with this type! Get the component by name.";
			msg = MessageFormat.format(msg, clazz.getName(), components.size());
			throw new IllegalArgumentException(msg);
		}

		return (T) components.getFirst();
	}

	@Override
	public <T extends StrolchComponent> T getComponentByName(String name) throws IllegalArgumentException {
		@SuppressWarnings("unchecked") T component = (T) this.componentsByName.get(name);
		if (component == null) {
			String msg = "The component with name {0} does not exist!";
			msg = MessageFormat.format(msg, name);
			throw new IllegalArgumentException(msg);
		}
		return component;
	}

	@Override
	public List<StrolchComponent> getComponentsOrderedByRoot() {
		DBC.PRE.assertNotNull("Not yet started!", this.dependencyAnalyzer);
		List<StrolchComponent> result = new ArrayList<>();
		List<StrolchComponent> components = new ArrayList<>(this.componentsByType.values());
		components.sort(Comparator.comparing(StrolchComponent::getName));

		while (!components.isEmpty()) {
			for (Iterator<StrolchComponent> iterator = components.iterator(); iterator.hasNext(); ) {
				StrolchComponent component = iterator.next();
				ComponentConfiguration configuration = strolchConfiguration.getComponentConfiguration(
						component.getName());
				Set<String> dependencies = configuration.getDependencies();
				if (!dependencies.isEmpty() && dependencies
						.stream()
						.map(this.componentsByName::get)
						.noneMatch(result::contains))
					continue;

				result.add(component);
				iterator.remove();
			}
		}

		return result;
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
				String msg
						= "The User {0} is missing the property {1} and the Realm {2} can not be used as it does not exist!";
				throw new StrolchException(
						MessageFormat.format(msg, certificate.getUsername(), PROP_REALM, DEFAULT_REALM));
			}
		}

		try {
			return getComponent(RealmHandler.class).getRealm(realmName);
		} catch (StrolchException e) {
			String msg
					= "The User {0} has property {1} with value={2}, but the Realm does not eixst, or is not accessible by this user!";
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

	private StrolchComponent setupComponent(MapOfLists<Class<?>, StrolchComponent> componentMap,
			Map<String, ComponentController> controllerMap, ComponentConfiguration componentConfiguration) {

		String componentName = componentConfiguration.getName();
		if (isEmpty(componentName))
			throw new IllegalStateException("name missing for a component in env " + componentConfiguration
					.getRuntimeConfiguration()
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
				String msg
						= "Component {0} has invalid configuration: Impl class {1} is not assignable to Api class {2}";
				msg = MessageFormat.format(msg, componentName, impl, api);
				throw new StrolchConfigurationException(msg);
			}

			if (!StrolchComponent.class.isAssignableFrom(implClass)) {
				String msg = "Component {0} has invalid configuration: Impl class {1} is not a subclass of {2}";
				msg = MessageFormat.format(msg, componentName, impl, StrolchComponent.class.getName());
				throw new StrolchConfigurationException(msg);
			}

			@SuppressWarnings("unchecked") Class<StrolchComponent> strolchComponentClass
					= (Class<StrolchComponent>) implClass;
			Constructor<StrolchComponent> constructor = strolchComponentClass.getConstructor(ComponentContainer.class,
					String.class);
			StrolchComponent strolchComponent = constructor.newInstance(this, componentName);
			strolchComponent.setup(componentConfiguration);

			componentMap.addElement(apiClass, strolchComponent);
			controllerMap.put(componentName, new ComponentController(strolchComponent));

			return strolchComponent;

		} catch (NoSuchMethodException e) {

			String msg
					= "Could not load class for component {0} due to missing constructor with signature (ComponentContainer.class, String.class)";
			msg = MessageFormat.format(msg, componentName);
			throw new StrolchConfigurationException(msg, e);

		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | SecurityException |
				 IllegalArgumentException | InvocationTargetException e) {

			String msg = "Could not load class for component {0} due to: {1}";
			msg = MessageFormat.format(msg, componentName, getRootCauseMessage(e));
			throw new StrolchConfigurationException(msg, e);
		}
	}

	public void setup(StrolchConfiguration strolchConfiguration) {
		this.strolchConfiguration = strolchConfiguration;
		this.state.validateStateChange(ComponentState.SETUP, "agent");

		long start = System.nanoTime();

		// set the application locale
		Locale.setDefault(strolchConfiguration.getRuntimeConfiguration().getLocale());
		String msg = "Application {0}:{1} is using locale {2} and timezone {3}";
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		System.setProperty("user.timezone", getTimezone());
		logger.info(MessageFormat.format(msg, applicationName, environment, Locale.getDefault(),
				System.getProperty("user.timezone")));

		// set up the container itself
		MapOfLists<Class<?>, StrolchComponent> componentMap = new MapOfLists<>();
		Map<String, StrolchComponent> componentsByName = new HashMap<>();
		Map<String, ComponentController> controllerMap = new HashMap<>();
		Set<String> componentNames = strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration.getComponentConfiguration(
					componentName);

			// setup each component
			StrolchComponent component = setupComponent(componentMap, controllerMap, componentConfiguration);
			componentsByName.put(component.getName(), component);
		}

		// then analyze dependencies
		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, controllerMap);
		this.dependencyAnalyzer.setupDependencies();

		// now save references
		this.componentsByType = componentMap;
		this.componentsByName = componentsByName;
		this.controllerMap = controllerMap;

		// and configure the state handler
		this.containerStateHandler = new ComponentContainerStateHandler(this.dependencyAnalyzer, strolchConfiguration);

		this.state = ComponentState.SETUP;

		long took = System.nanoTime() - start;
		msg = "{0}:{1} Strolch Container setup with {2} components. Took {3}";
		logger.info(MessageFormat.format(msg, applicationName, environment, this.componentsByType.size(),
				formatNanoDuration(took)));
	}

	public void initialize() {
		this.state.validateStateChange(ComponentState.INITIALIZED, "agent");

		long start = System.nanoTime();

		// now we can initialize the components
		String msg = "{0}:{1} Initializing {2} Strolch Components...";
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		containerStateHandler.initialize(rootUpstreamComponents);

		this.state = ComponentState.INITIALIZED;

		long took = System.nanoTime() - start;
		msg = "{0}:{1} All {2} Strolch Components have been initialized. Took {3}";
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
				formatNanoDuration(took)));
	}

	public void start() {
		this.state.validateStateChange(ComponentState.STARTED, "agent");

		long start = System.nanoTime();

		String msg = "{0}:{1} Starting {2} Strolch Components...";
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
		logger.info(MessageFormat.format("file.encoding: {0} / sun.jnu.encoding {1}",
				Charset.defaultCharset().displayName(), System.getProperty("sun.jnu.encoding")));

		String tookS = formatNanoDuration(System.nanoTime() - start);

		if (hasComponent(OperationsLog.class)) {
			for (String realmName : getRealmNames()) {
				getComponent(OperationsLog.class).addMessage(
						new LogMessage(realmName, SYSTEM_USER_AGENT, Locator.valueOf(AGENT, AGENT, "start"),
								LogSeverity.System, LogMessageState.Information,
								ResourceBundle.getBundle("strolch-agent"), "agent.started") //
								.value("applicationName", applicationName) //
								.value("environment", environment) //
								.value("components", String.valueOf(this.controllerMap.size())) //
								.value("took", tookS));
			}
		}

		msg
				= "{0}:{1} All {2} Strolch Components started for version {3}. Took {4}. Strolch is now ready to be used. Have fun =))";
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
				getAgent().getVersion().getAppVersion().getArtifactVersion(), tookS));
	}

	public void stop() {
		this.state.validateStateChange(ComponentState.STOPPED, "agent");

		long start = System.nanoTime();

		String environment = getEnvironment();
		String applicationName = getApplicationName();

		if (hasComponent(OperationsLog.class)) {
			for (String realmName : getRealmNames()) {
				getComponent(OperationsLog.class).addMessage(
						new LogMessage(realmName, SYSTEM_USER_AGENT, Locator.valueOf(AGENT, AGENT, "stop"),
								LogSeverity.System, LogMessageState.Information,
								ResourceBundle.getBundle("strolch-agent"), "agent.stopping") //
								.value("applicationName", applicationName) //
								.value("environment", environment) //
								.value("components", String.valueOf(this.controllerMap.size())));
			}
		}

		String msg = "{0}:{1} Stopping {2} Strolch Components...";
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to stop");
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.stop(rootUpstreamComponents);

			long took = System.nanoTime() - start;
			msg = "{0}:{1} All {2} Strolch Components have been stopped. Took {3}";
			logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
					formatNanoDuration(took)));
		}

		this.state = ComponentState.STOPPED;
	}

	public void destroy() {
		this.state.validateStateChange(ComponentState.DESTROYED, "agent");

		long start = System.nanoTime();

		String msg = "{0}:{1} Destroying {2} Strolch Components...";
		String environment = getEnvironment();
		String applicationName = getApplicationName();
		logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to destroy");
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.destroy(rootUpstreamComponents);

			long took = System.nanoTime() - start;
			msg = "{0}:{1} All {2} Strolch Components have been destroyed! Took {3}";
			logger.info(MessageFormat.format(msg, applicationName, environment, this.controllerMap.size(),
					formatNanoDuration(took)));
			this.controllerMap.clear();
			this.componentsByType.clear();
		}

		this.state = ComponentState.DESTROYED;

		this.controllerMap = null;
		this.componentsByType = null;
	}

	private String getApplicationName() {
		return getAgent().getApplicationName();
	}

	private String getEnvironment() {
		return getAgent().getStrolchConfiguration().getRuntimeConfiguration().getEnvironment();
	}

	private String getTimezone() {
		return getAgent()
				.getStrolchConfiguration()
				.getRuntimeConfiguration()
				.getString(PROP_TIMEZONE, System.getProperty("user.timezone"));
	}
}
