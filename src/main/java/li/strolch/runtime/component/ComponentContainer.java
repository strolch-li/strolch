package li.strolch.runtime.component;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.HashMap;
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
	private Map<String, ComponentController> controllerMap;
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private StrolchConfiguration strolchConfiguration;
	private ComponentState state;

	public ComponentContainer() {
		this.state = ComponentState.UNDEFINED;
	}

	public ComponentState getState() {
		return this.state;
	}

	public boolean hasComponent(Class<?> clazz) {
		return this.componentMap.containsKey(clazz);
	}

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

		Map<Class<?>, StrolchComponent> componentMap = new HashMap<>();
		Map<String, ComponentController> controllerMap = new HashMap<>();
		Set<String> componentNames = strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = strolchConfiguration
					.getComponentConfiguration(componentName);
			initializeComponent(componentMap, controllerMap, componentConfiguration);
		}

		this.dependencyAnalyzer = new ComponentDependencyAnalyzer(strolchConfiguration, controllerMap);
		this.dependencyAnalyzer.setupDependencies();

		this.componentMap = componentMap;
		this.controllerMap = controllerMap;
		this.strolchConfiguration = strolchConfiguration;

		msg = "Strolch Container setup with {0} components."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.componentMap.size()));
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
			Constructor<StrolchComponent> constructor = strolchComponentClass.getConstructor(ComponentContainer.class,
					String.class);
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

	public void initialize() {

		logger.info("Initializing strolch components..."); //$NON-NLS-1$

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootUpstreamComponents();
		initialize(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.INITIALIZED);

		String msg = "All {0} Strolch Components have been initialized."; //$NON-NLS-1$
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

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
		stop(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.STOPPED);

		String msg = "All {0} Strolch Components have been stopped."; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.controllerMap.size()));
	}

	public void destroy() {

		logger.info("Destroying strolch components..."); //$NON-NLS-1$

		Set<ComponentController> rootUpstreamComponents = this.dependencyAnalyzer.findRootDownstreamComponents();
		destroy(rootUpstreamComponents);
		this.state = this.state.validateStateChange(ComponentState.DESTROYED);

		String msg = "All {0} Strolch Components have been destroyed!"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, this.controllerMap.size()));
		this.controllerMap.clear();
		this.componentMap.clear();
	}
}
