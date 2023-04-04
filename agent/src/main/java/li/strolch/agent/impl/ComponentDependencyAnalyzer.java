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

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;

public class ComponentDependencyAnalyzer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentDependencyAnalyzer.class);
	private final StrolchConfiguration strolchConfiguration;
	private final Map<String, ComponentController> controllerMap;

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

	private Set<ComponentController> collectAllUpstreamDependencies(ComponentController controller) {
		Set<ComponentController> upstreamDependencies = new HashSet<>(controller.getUpstreamDependencies());
		for (ComponentController upstream : controller.getUpstreamDependencies()) {
			upstreamDependencies.addAll(collectAllUpstreamDependencies(upstream));
		}

		return upstreamDependencies;
	}

	public Set<ComponentController> collectDirectUpstreamDependencies(Set<ComponentController> controllers) {

		// assert no upstream is in this list
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = collectAllUpstreamDependencies(controller);
			for (ComponentController upstream : upstreamDependencies) {
				DBC.INTERIM.assertFalse("Upstream " + upstream.getName() + " is one of the input controllers!",
						controllers.contains(upstream));
			}
		}

		Set<ComponentController> directUpstreamDependencies = new HashSet<>();

		// collect all direct upstream dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = controller.getUpstreamDependencies();
			directUpstreamDependencies.addAll(upstreamDependencies);
		}

		// prune any controllers which are an upstream dependency of any of these dependencies
		Set<ComponentController> tmp = new HashSet<>(directUpstreamDependencies);
		for (ComponentController controller : tmp) {
			Set<ComponentController> upstreamDeps = collectAllUpstreamDependencies(controller);
			directUpstreamDependencies.removeAll(upstreamDeps);
		}

		return directUpstreamDependencies;
	}

	private Set<ComponentController> collectAllDownstreamDependencies(ComponentController controller) {
		Set<ComponentController> downstreamDependencies = new HashSet<>(controller.getDownstreamDependencies());
		for (ComponentController downstream : controller.getDownstreamDependencies()) {
			downstreamDependencies.addAll(collectAllDownstreamDependencies(downstream));
		}

		return downstreamDependencies;
	}

	public Set<ComponentController> collectDirectDownstreamDependencies(Set<ComponentController> controllers) {

		// assert no downstream is in this list
		for (ComponentController controller : controllers) {
			Set<ComponentController> downstreamDependencies = collectAllUpstreamDependencies(controller);
			for (ComponentController downstream : downstreamDependencies) {
				DBC.INTERIM.assertFalse("Downstream " + downstream.getName() + " is one of the input controllers!",
						controllers.contains(downstream));
			}
		}

		Set<ComponentController> directDownstreamDependencies = new HashSet<>();

		// collect all direct downstream dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> downstreamDependencies = controller.getDownstreamDependencies();
			directDownstreamDependencies.addAll(downstreamDependencies);
		}

		// prune any controllers which are a downstream dependency of any of these dependencies
		Set<ComponentController> tmp = new HashSet<>(directDownstreamDependencies);
		for (ComponentController controller : tmp) {
			Set<ComponentController> downstreamDeps = collectAllDownstreamDependencies(controller);
			directDownstreamDependencies.removeAll(downstreamDeps);
		}

		return directDownstreamDependencies;
	}

	public void setupDependencies() {
		for (ComponentController controller : this.controllerMap.values()) {
			String name = controller.getComponent().getName();
			ComponentConfiguration configuration = this.strolchConfiguration.getComponentConfiguration(name);

			Set<String> dependencies = configuration.getDependencies();
			for (String dependencyName : dependencies) {
				ComponentController dependency = this.controllerMap.get(dependencyName);
				if (dependency == null) {
					String msg = "Component {0} is missing required dependency {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, name, dependencyName);
					throw new StrolchConfigurationException(msg);
				}
				controller.addUpstreamDependency(dependency);
			}
		}

		logDependencies(1, findRootUpstreamComponents());
	}

	/**
	 * @param components
	 */
	private void logDependencies(int depth, Set<ComponentController> components) {
		if (depth == 1) {
			logger.info("Dependency tree:"); //$NON-NLS-1$
		}
		String inset = StringHelper.normalizeLength("", depth * 2, false, ' '); //$NON-NLS-1$
		for (ComponentController controller : components) {
			logger.info(inset + controller.getComponent().getName() + ": "
					+ controller.getComponent().getClass().getName());
			logDependencies(depth + 1, controller.getDownstreamDependencies());
		}
	}
}
