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
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.ComponentState;
import li.strolch.agent.api.ElementMapHandler;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComponentContainerImpl implements ComponentContainer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerImpl.class);

	private StrolchAgent agent;
	private DataStoreMode dataStoreMode;

	private Map<Class<?>, StrolchComponent> componentMap;
	private Map<String, ComponentController> controllerMap;
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private StrolchConfiguration strolchConfiguration;
	private ComponentState state;

	public ComponentContainerImpl(StrolchAgent agent, DataStoreMode dataStoreMode) {
		this.agent = agent;
		this.dataStoreMode = dataStoreMode;
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
	public DataStoreMode getDataStoreMode() {
		return this.dataStoreMode;
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
	public <T> T getComponent(Class<T> clazz) {
		T component = (T) this.componentMap.get(clazz);
		if (component == null) {
			String msg = "The component does not exist for class {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, clazz);
			throw new IllegalArgumentException(msg);
		}
		return component;
	}

	@Override
	public Set<String> getRealmNames() {
		return getComponent(ElementMapHandler.class).getRealmNames();
	}

	@Override
	public StrolchRealm getDefaultRealm() {
		return getRealm(StrolchConstants.DEFAULT_REALM);
	}

	@Override
	public StrolchRealm getRealm(String realm) {
		return getComponent(ElementMapHandler.class).getRealm(realm);
	}

	private void initializeComponent(Map<Class<?>, StrolchComponent> componentMap,
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
			Constructor<StrolchComponent> constructor = strolchComponentClass.getConstructor(
					ComponentContainer.class, String.class);
			StrolchComponent strolchComponent = constructor.newInstance(this, componentName);

			componentMap.put(apiClass, strolchComponent);
			controllerMap.put(componentName, new ComponentController(strolchComponent));
			String msg = "Initialized component {0} with API {1} and impl {2}."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, componentName, api, impl));

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

	private void initialize(Set<ComponentController> controllers) {

		// initialize each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.INITIALIZED)
				continue;

			StrolchComponent component = controller.getComponent();
			String componentName = component.getName();
			ComponentConfiguration componentConfiguration = this.strolchConfiguration
					.getComponentConfiguration(componentName);

			String msg = "Initializing component {0}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, componentName));
			component.initialize(componentConfiguration);
		}

		// initialize direct downstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer
				.collectDirectDownstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			initialize(dependencies);
	}

	private void start(Set<ComponentController> controllers) {

		// Start each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.STARTED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Starting component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			component.start();
		}

		// Start direct downstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer
				.collectDirectDownstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			start(dependencies);
	}

	private void stop(Set<ComponentController> controllers) {

		// Stop each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.STOPPED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Stopping component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			component.stop();
		}

		// Stop direct upstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer.collectDirectUpstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			stop(dependencies);
	}

	private void destroy(Set<ComponentController> controllers) {

		// Destroy each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.DESTROYED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Destroying component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			component.destroy();
		}

		// Destroy direct upstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer.collectDirectUpstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			destroy(dependencies);
	}

	public void initialize(StrolchConfiguration strolchConfiguration) {

		// first set up the container itself
		this.strolchConfiguration = strolchConfiguration;
		Map<Class<?>, StrolchComponent> componentMap = new HashMap<>();
		Map<String, ComponentController> controllerMap = new HashMap<>();
		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration
					.getComponentConfiguration(componentName);
			initializeComponent(componentMap, controllerMap, componentConfiguration);
		}

		// then analyze dependencies
		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, controllerMap);
		this.dependencyAnalyzer.setupDependencies();

		// now save references
		this.componentMap = componentMap;
		this.controllerMap = controllerMap;
		this.strolchConfiguration = strolchConfiguration;

		String msg = "Strolch Container setup with {0} components."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));

		// now we can initialize the components
		logger.info("Initializing strolch components..."); //$NON-NLS-1$

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		initialize(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);

		msg = "All {0} Strolch Components have been initialized."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.controllerMap.size()));
	}

	public void start() {

		logger.info("Starting strolch components..."); //$NON-NLS-1$

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		start(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.STARTED);

		String msg = "All {0} Strolch Components started and container now ready to be used. Have fun =))"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.controllerMap.size()));
	}

	public void stop() {

		logger.info("Stopping strolch components..."); //$NON-NLS-1$

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch Components have been stopped."); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			stop(rootUpstreamComponents);
			this.state = this.state.validateStateChange(ComponentState.STOPPED);

			String msg = "All {0} Strolch Components have been stopped."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.controllerMap.size()));
		}
	}

	public void destroy() {

		logger.info("Destroying strolch components..."); //$NON-NLS-1$

		if (this.dependencyAnalyzer == null) {
			logger.info("Strolch Components have been destroyed."); //$NON-NLS-1$
		} else {
			Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
			destroy(rootUpstreamComponents);
			this.state = this.state.validateStateChange(ComponentState.DESTROYED);

			String msg = "All {0} Strolch Components have been destroyed!"; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, this.controllerMap.size()));
			this.controllerMap.clear();
			this.componentMap.clear();
		}

		this.controllerMap = null;
		this.componentMap = null;
	}
}
