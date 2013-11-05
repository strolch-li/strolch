package li.strolch.runtime.component;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

public class ComponentDependencyAnalyzer {

	private StrolchConfiguration strolchConfiguration;
	private Map<String, StrolchComponent> componentMap;

	public ComponentDependencyAnalyzer(StrolchConfiguration strolchConfiguration,
			Map<String, StrolchComponent> componentMap) {
		this.strolchConfiguration = strolchConfiguration;
		this.componentMap = componentMap;
	}

	public Set<StrolchComponent> findAllRootComponents() {

		Set<StrolchComponent> rootComponents = new HashSet<>();

		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = this.strolchConfiguration
					.getComponentConfiguration(componentName);
			if (componentConfiguration.getDependencies().isEmpty()) {
				StrolchComponent strolchComponent = this.componentMap.get(componentName);
				rootComponents.add(strolchComponent);
			}
		}

		return rootComponents;
	}

	public Set<StrolchComponent> findAllLeafComponents() {

		Set<StrolchComponent> leafComponents = new HashSet<>();

		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {

			boolean noDownStreamDependency = true;
			for (String possibleDependency : componentNames) {
				if (componentName.equals(possibleDependency))
					continue;

				ComponentConfiguration dependency = this.strolchConfiguration
						.getComponentConfiguration(possibleDependency);
				if (dependency.getDependencies().contains(componentName))
					noDownStreamDependency = false;
			}

			if (noDownStreamDependency) {
				StrolchComponent strolchComponent = this.componentMap.get(componentName);
				leafComponents.add(strolchComponent);
			}
		}

		return leafComponents;
	}

	public Set<StrolchComponent> findUpstreamDependencies(StrolchComponent component) {

		String componentName = component.getName();
		ComponentConfiguration componentConfiguration = this.strolchConfiguration
				.getComponentConfiguration(componentName);

		Set<String> dependencyNames = componentConfiguration.getDependencies();
		if (dependencyNames.isEmpty())
			return Collections.emptySet();

		Set<StrolchComponent> dependencies = new HashSet<>(dependencyNames.size());
		for (String dependencyName : dependencyNames) {
			StrolchComponent dependency = this.componentMap.get(dependencyName);
			if (dependency == null) {
				String msg = "The dependency {0} for component {0} does not exist!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, dependencyName, componentName);
				throw new StrolchConfigurationException(msg);
			}

			dependencies.add(dependency);
		}

		return dependencies;
	}

	public Set<StrolchComponent> findDownstreamDependencies(StrolchComponent component) {

		String componentName = component.getName();
		Set<StrolchComponent> dependencies = new HashSet<>();

		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String name : componentNames) {
			ComponentConfiguration configuration = this.strolchConfiguration.getComponentConfiguration(name);
			if (configuration.getDependencies().contains(componentName))
				dependencies.add(this.componentMap.get(name));
		}

		return dependencies;
	}

	public void assertHasNoCyclicDependency() {

		Set<String> componentNames = this.strolchConfiguration.getComponentNames();
		for (String componentName : componentNames) {
			ComponentConfiguration componentConfiguration = this.strolchConfiguration
					.getComponentConfiguration(componentName);
			List<String> cyclicDependencies = new ArrayList<>();
			findCyclicDependency(cyclicDependencies, componentName, componentConfiguration.getDependencies());
			if (!cyclicDependencies.isEmpty()) {
				String msg = "Found a cyclic dependency for component {0}: {1}"; //$NON-NLS-1$
				StringBuilder sb = new StringBuilder(componentName);
				for (String dep : cyclicDependencies) {
					sb.append(" -> "); //$NON-NLS-1$
					sb.append(dep);
				}
				msg = MessageFormat.format(msg, componentName, sb.toString());
				throw new StrolchConfigurationException(msg);
			}
		}
	}

	private void findCyclicDependency(List<String> cyclicDependencies, String componentName, Set<String> dependencies) {

		for (String dependency : dependencies) {
			if (componentName.equals(dependency)) {
				cyclicDependencies.add(dependency);
				return;
			}

			ComponentConfiguration dependencyConfiguration = this.strolchConfiguration
					.getComponentConfiguration(dependency);
			Set<String> nextDependencies = dependencyConfiguration.getDependencies();
			findCyclicDependency(cyclicDependencies, componentName, nextDependencies);
		}
	}

}
