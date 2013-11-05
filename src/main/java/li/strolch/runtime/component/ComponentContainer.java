package li.strolch.runtime.component;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComponentContainer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainer.class);

	private Map<Class<?>, StrolchComponent> componentMap;

	private ComponentDependencyAnalyzer dependencyAnalyzer;

	private StrolchConfiguration strolchConfiguration;

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

	public void setup(File path) {

		String msg = "Setting up Strolch Container from root {0}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, path.getAbsolutePath()));

		StrolchConfiguration strolchConfiguration = ConfigurationParser.parseConfiguration(path);

		Map<Class<?>, StrolchComponent> componentByApiMap = new HashMap<>();
		Map<String, StrolchComponent> componentByNameMap = new HashMap<>();
		Set<String> componentNames = strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration
					.getComponentConfiguration(componentName);
			initializeComponent(componentByApiMap, componentByNameMap, componentConfiguration);
		}

		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, componentByNameMap);
		this.dependencyAnalyzer.assertHasNoCyclicDependency();

		this.componentMap = componentByApiMap;
		this.strolchConfiguration = strolchConfiguration;
		msg = "Strolch Container setup with {0} components."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
	}

	private void initializeComponent(Map<Class<?>, StrolchComponent> componentByApiMap,
			Map<String, StrolchComponent> componentByNameMap, ComponentConfiguration componentConfiguration) {

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

			componentByApiMap.put(apiClass, strolchComponent);
			componentByNameMap.put(componentName, strolchComponent);
			String msg = "Initialized component {0} with API {1} and impl {2}."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, componentName, api, impl));

		} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | NoSuchMethodException
				| SecurityException | IllegalArgumentException | InvocationTargetException e) {

			String msg = "Could not load class for component {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, componentName, e.getMessage());
			throw new StrolchConfigurationException(msg, e);
		}
	}

	public void start(Set<StrolchComponent> components) {

		Set<StrolchComponent> allDownstreamDependencies = findDirectDownstreamDependencies(components);

		// initialize each component
		for (StrolchComponent component : components) {
			component.start();
		}

		// initialize downstream dependencies
		start(allDownstreamDependencies);
	}

	public void stop(Set<StrolchComponent> components) {

		Set<StrolchComponent> allDownstreamDependencies = findDirectUpstreamDependencies(components);

		// initialize each component
		for (StrolchComponent component : components) {
			component.stop();
		}

		// initialize downstream dependencies
		stop(allDownstreamDependencies);
	}

	public void destroy(Set<StrolchComponent> components) {

		Set<StrolchComponent> allUpstreamDependencies = findDirectUpstreamDependencies(components);

		// initialize each component
		for (StrolchComponent component : components) {
			component.destroy();
		}

		// initialize downstream dependencies
		destroy(allUpstreamDependencies);
	}

	private Set<StrolchComponent> findDirectDownstreamDependencies(Set<StrolchComponent> components) {
		Set<StrolchComponent> allDownstreamDependencies = new HashSet<>();

		// find the direct downstream dependencies
		for (StrolchComponent component : components) {

			// find this component's downstream dependencies
			Set<StrolchComponent> downstreamDependencies = this.dependencyAnalyzer
					.findDownstreamDependencies(component);

			// add all of these downstream dependencies
			allDownstreamDependencies.addAll(downstreamDependencies);
		}

		// make sure that in these downstream dependencies
		// none of these dependencies depend on each other
		Set<StrolchComponent> tmpDependencies = new HashSet<>(allDownstreamDependencies);
		for (StrolchComponent component : tmpDependencies) {

			Set<StrolchComponent> upstreamDependencies = this.dependencyAnalyzer.findUpstreamDependencies(component);
			allDownstreamDependencies.removeAll(upstreamDependencies);
		}
		return allDownstreamDependencies;
	}

	private Set<StrolchComponent> findDirectUpstreamDependencies(Set<StrolchComponent> components) {
		Set<StrolchComponent> allUpstreamDependencies = new HashSet<>();

		// TODO

		return allUpstreamDependencies;
	}

	public void initialize() {

		logger.info("Initializing strolch components..."); //$NON-NLS-1$

		Collection<StrolchComponent> components = this.componentMap.values();
		for (StrolchComponent component : components) {
			String msg = "Initializing component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			ComponentConfiguration configuration = this.strolchConfiguration.getComponentConfiguration(componentName);
			component.initialize(configuration);
		}

		String msg = "All {0} Strolch Components have been initialized."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
	}

	public void start() {

		logger.info("Starting strolch components..."); //$NON-NLS-1$

		Collection<StrolchComponent> components = this.componentMap.values();
		for (StrolchComponent strolchComponent : components) {
			String msg = "Starting component {0}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, strolchComponent.getName()));
			strolchComponent.start();
		}

		String msg = "All {0} Strolch Components started and container now ready to be used. Have fun =))"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
	}

	public void stop() {

		logger.info("Stopping strolch components..."); //$NON-NLS-1$

		Collection<StrolchComponent> components = this.componentMap.values();
		for (StrolchComponent strolchComponent : components) {
			String msg = "Stopping component {0}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, strolchComponent.getName()));
			strolchComponent.stop();
		}

		String msg = "All {0} Strolch Components have been stopped."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
	}

	public void destroy() {

		logger.info("Destroying strolch components..."); //$NON-NLS-1$

		Collection<StrolchComponent> components = this.componentMap.values();
		for (StrolchComponent strolchComponent : components) {
			String msg = "Destroying component {0}..."; //$NON-NLS-1$
			logger.info(MessageFormat.format(msg, strolchComponent.getName()));
			strolchComponent.destroy();
		}

		String msg = "All {0} Strolch Components have been destroyed!"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
		this.componentMap.clear();
	}
}
