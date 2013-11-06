package li.strolch.runtime.component;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;

public class ComponentDependencyAnalyzer {

	private StrolchConfiguration strolchConfiguration;
	private Map<String, ComponentController> controllerMap;

	public ComponentDependencyAnalyzer(StrolchConfiguration strolchConfiguration,
			Map<String, ComponentController> controllerMap) {
		this.strolchConfiguration = strolchConfiguration;
		this.controllerMap = controllerMap;
	}

	public Set<ComponentController> findRootUpstreamComponents() {
		Set<ComponentController> controllers = new HashSet<>();
		for (ComponentController controller : this.controllerMap.values()) {
			if (!controller.hasUpstreamDependencies())
				controllers.add(controller);
		}

		return controllers;
	}

	public Set<ComponentController> findRootDownstreamComponents() {
		Set<ComponentController> controllers = new HashSet<>();
		for (ComponentController controller : this.controllerMap.values()) {
			if (!controller.hasDownstreamDependencies())
				controllers.add(controller);
		}

		return controllers;
	}

	public Set<ComponentController> findDirectUpstreamDependencies(Set<ComponentController> controllers) {

		Set<ComponentController> directUpstreamDependencies = new HashSet<>();

		// collect all direct upstream dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = controller.getUpstreamDependencies();
			directUpstreamDependencies.addAll(upstreamDependencies);
		}
		
		// assert no dependency in list which was from source
		//directUpstreamDependencies.

		// prune dependencies which are a dependency of any of these dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = controller.getUpstreamDependencies();

			for (ComponentController upstream : upstreamDependencies) {

				Iterator<ComponentController> iter = directUpstreamDependencies.iterator();
				while (iter.hasNext()) {
					ComponentController possibleTransitiveDependency = iter.next();
					if (upstream.hasUpstreamDependency(possibleTransitiveDependency))
						continue;

					if (upstream.hasTransitiveUpstreamDependency(possibleTransitiveDependency))
						iter.remove();
				}
			}
		}

		return directUpstreamDependencies;
	}

	public Set<ComponentController> findDirectDownstreamDependencies(ComponentController component) {

		Set<ComponentController> controllers = new HashSet<>();

		return controllers;
	}

	public void setupDependencies() {
		for (ComponentController controller : this.controllerMap.values()) {
			String name = controller.getComponent().getName();
			ComponentConfiguration configuration = this.strolchConfiguration.getComponentConfiguration(name);
			Set<String> dependencies = configuration.getDependencies();
			for (String dependency : dependencies) {

			}
		}
	}
}
