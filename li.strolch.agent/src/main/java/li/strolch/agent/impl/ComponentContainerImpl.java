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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ComponentState;
import li.strolch.agent.api.RealmHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.runtime.privilege.PrivilegeHandler;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.utils.helper.StringHelper;
import ch.eitchnet.utils.helper.SystemHelper;

public class ComponentContainerImpl implements ComponentContainer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerImpl.class);

	private StrolchAgent agent;
	private Map<Class<?>, StrolchComponent> componentMap;
	private Map<String, ComponentController> controllerMap;
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private StrolchConfiguration strolchConfiguration;
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
			msg = MessageFormat.format(msg, clazz);
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

		String realmName = certificate.getProperty(StrolchConstants.PROP_REALM);
		if (StringHelper.isEmpty(realmName)) {
			String msg = "The User {0} is missing the property {1}";
			throw new StrolchException(
					MessageFormat.format(msg, certificate.getUsername(), StrolchConstants.PROP_REALM));
		}

		try {
			return getComponent(RealmHandler.class).getRealm(realmName);
		} catch (StrolchException e) {
			String msg = "The User {0} has property {1} with value={2}, but the Realm does not eixst, or is not accessible by this user!";
			throw new StrolchException(MessageFormat.format(msg, certificate.getUsername(),
					StrolchConstants.PROP_REALM, realmName), e);
		}
	}

	private void setupComponent(Map<Class<?>, StrolchComponent> componentMap,
			Map<String, ComponentController> controllerMap, ComponentConfiguration componentConfiguration) {

		String componentName = componentConfiguration.getName();
		try {
			String api = componentConfiguration.getApi();
			String impl = componentConfiguration.getImpl();
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

		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | SecurityException
				| IllegalArgumentException | InvocationTargetException e) {

			String msg = "Could not load class for component {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, componentName, e.getMessage());
			throw new StrolchConfigurationException(msg, e);
		}
	}

	public void setup(StrolchConfiguration strolchConfiguration) {

		// set the application locale
		RuntimeConfiguration runConf = strolchConfiguration.getRuntimeConfiguration();
		Locale locale = runConf.getLocale();
		Locale.setDefault(locale);
		String localeMsg = "Application {0}:{1} is using locale {2}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(localeMsg, runConf.getApplicationName(), runConf.getEnvironment(),
				Locale.getDefault()));

		// set up the container itself
		this.strolchConfiguration = strolchConfiguration;
		Map<Class<?>, StrolchComponent> componentMap = new HashMap<>();
		Map<String, ComponentController> controllerMap = new HashMap<>();
		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration
					.getComponentConfiguration(componentName);
			setupComponent(componentMap, controllerMap, componentConfiguration);
		}

		// then analyze dependencies
		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, controllerMap);
		this.dependencyAnalyzer.setupDependencies();

		// now save references
		this.componentMap = componentMap;
		this.controllerMap = controllerMap;
		this.strolchConfiguration = strolchConfiguration;

		// and configure the state handler
		this.containerStateHandler = new ComponentContainerStateHandler(this.dependencyAnalyzer,
				this.strolchConfiguration);

		this.state = this.state.validateStateChange(ComponentState.SETUP);
		String msg = "{0}:{1} Strolch Container setup with {2} components."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.componentMap.size()));
	}

	public void initialize(StrolchConfiguration strolchConfiguration) {

		// now we can initialize the components
		String msg = "{0}:{1} Initializing {2} Strolch Components..."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		containerStateHandler.initialize(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);

		msg = "{0}:{1} All {2} Strolch Components have been initialized."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));
	}

	public void start() {

		String msg = "{0}:{1} Starting {2} Strolch Components..."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		containerStateHandler.start(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.STARTED);

		msg = "{0}:{1} All {2} Strolch Components started. Strolch is now ready to be used. Have fun =))"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));
		logger.info(MessageFormat.format("System: {0}", SystemHelper.asString())); //$NON-NLS-1$
		logger.info(MessageFormat.format("Memory: {0}", SystemHelper.getMemorySummary())); //$NON-NLS-1$
	}

	public void stop() {

		String msg = "{0}:{1} Stopping {2} Strolch Components..."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to stop"); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.stop(rootUpstreamComponents);
			this.state = this.state.validateStateChange(ComponentState.STOPPED);

			msg = "{0}:{1} All {2} Strolch Components have been stopped."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
					.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));
		}
	}

	public void destroy() {

		String msg = "{0}:{1} Destroying {2} Strolch Components..."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
				.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch was not yet setup, nothing to destroy"); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			containerStateHandler.destroy(rootUpstreamComponents);
			this.state = this.state.validateStateChange(ComponentState.DESTROYED);

			msg = "{0}:{1} All {2} Strolch Components have been destroyed!"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, getAgent().getApplicationName(), getAgent().getStrolchConfiguration()
					.getRuntimeConfiguration().getEnvironment(), this.controllerMap.size()));
			this.controllerMap.clear();
			this.componentMap.clear();
		}

		this.controllerMap = null;
		this.componentMap = null;
	}
}
