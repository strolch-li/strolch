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
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ComponentState;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.exception.StrolchException;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ComponentContainerStateHandler {

	private static final Logger logger = LoggerFactory.getLogger(ComponentContainerStateHandler.class);
	private ComponentDependencyAnalyzer dependencyAnalyzer;
	private StrolchConfiguration strolchConfiguration;

	public ComponentContainerStateHandler(ComponentDependencyAnalyzer dependencyAnalyzer,
			StrolchConfiguration strolchConfiguration) {
		this.dependencyAnalyzer = dependencyAnalyzer;
		this.strolchConfiguration = strolchConfiguration;
	}

	public void initialize(Set<ComponentController> controllers) {

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
			try {
				component.initialize(componentConfiguration);
			} catch (Exception e) {
				throw new StrolchException(MessageFormat.format("Failed to initialize component {0}", componentName), e);
			}
		}

		// initialize direct downstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer
				.collectDirectDownstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			initialize(dependencies);
	}

	public void start(Set<ComponentController> controllers) {

		// Start each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.STARTED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Starting component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			try {
				component.start();
			} catch (Exception e) {
				throw new StrolchException(MessageFormat.format("Failed to start component {0}", componentName), e);
			}
		}

		// Start direct downstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer
				.collectDirectDownstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			start(dependencies);
	}

	public void stop(Set<ComponentController> controllers) {

		// Stop each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.STOPPED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Stopping component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			try {
				component.stop();
			} catch (Exception e) {
				msg = "Failed to stop component {0} due to {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, componentName, e.getMessage());
				logger.error(msg, e);
			}
		}

		// Stop direct upstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer.collectDirectUpstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			stop(dependencies);
	}

	public void destroy(Set<ComponentController> controllers) {

		// Destroy each component
		for (ComponentController controller : controllers) {
			if (controller.getState() == ComponentState.DESTROYED)
				continue;

			StrolchComponent component = controller.getComponent();
			String msg = "Destroying component {0}..."; //$NON-NLS-1$
			String componentName = component.getName();
			logger.info(MessageFormat.format(msg, componentName));
			try {
				component.destroy();
			} catch (Exception e) {
				msg = "Failed to destroy component {0} due to {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, componentName, e.getMessage());
				logger.error(msg, e);
			}
		}

		// Destroy direct upstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer.collectDirectUpstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			destroy(dependencies);
	}
}
