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
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.StringHelper;

public class ComponentDependencyAnalyzer {

	private static final Logger logger = LoggerFactory.getLogger(ComponentDependencyAnalyzer.class);
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

	public Set<ComponentController> collectDirectUpstreamDependencies(Set<ComponentController> controllers) {

		Set<ComponentController> directUpstreamDependencies = new HashSet<>();

		// collect all direct upstream dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = controller.getUpstreamDependencies();
			directUpstreamDependencies.addAll(upstreamDependencies);
		}

		// prune dependencies which are a dependency of any of these dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> upstreamDependencies = controller.getUpstreamDependencies();

			for (ComponentController upstream : upstreamDependencies) {

				Iterator<ComponentController> iter = directUpstreamDependencies.iterator();
				while (iter.hasNext()) {
					ComponentController possibleTransitiveDependency = iter.next();
					if (upstream.hasUpstreamDependency(possibleTransitiveDependency))
						continue;

					if (possibleTransitiveDependency.hasTransitiveUpstreamDependency(upstream))
						iter.remove();
				}
			}
		}

		return directUpstreamDependencies;
	}

	public Set<ComponentController> collectDirectDownstreamDependencies(Set<ComponentController> controllers) {

		Set<ComponentController> directDownstreamDependencies = new HashSet<>();

		// collect all direct downstream dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> downstreamDependencies = controller.getDownstreamDependencies();
			directDownstreamDependencies.addAll(downstreamDependencies);
		}

		// prune dependencies which are a dependency of any of these dependencies
		for (ComponentController controller : controllers) {
			Set<ComponentController> downstreamDependencies = controller.getDownstreamDependencies();

			for (ComponentController downstream : downstreamDependencies) {

				Iterator<ComponentController> iter = directDownstreamDependencies.iterator();
				while (iter.hasNext()) {
					ComponentController possibleTransitiveDependency = iter.next();
					if (downstream.hasUpstreamDependency(possibleTransitiveDependency))
						continue;

					if (possibleTransitiveDependency.hasTransitiveDownstreamDependency(downstream))
						iter.remove();
				}
			}
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

		logDependencies(0, findRootUpstreamComponents());
	}

	/**
	 * @param components
	 */
	private void logDependencies(int depth, Set<ComponentController> components) {
		if (depth == 0) {
			logger.info("Dependency tree:"); //$NON-NLS-1$
		}
		String inset = StringHelper.normalizeLength("  ", depth * 2, false, ' '); //$NON-NLS-1$
		for (ComponentController controller : components) {
			logger.info(inset + controller.getComponent().getName() + ": "
					+ controller.getComponent().getClass().getName());
			logDependencies(depth + 1, controller.getDownstreamDependencies());
		}
	}
}
