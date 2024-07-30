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

import static li.strolch.utils.helper.StringHelper.formatNanoDuration;

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
	private final ComponentDependencyAnalyzer dependencyAnalyzer;
	private final StrolchConfiguration strolchConfiguration;

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

			long start = System.nanoTime();

			StrolchComponent component = controller.getComponent();
			String componentName = component.getName();
			ComponentConfiguration componentConfiguration = this.strolchConfiguration
					.getComponentConfiguration(componentName);

			try {
				component.initialize(componentConfiguration);
			} catch (Exception e) {
				throw new StrolchException(MessageFormat.format("Failed to initialize component {0}", componentName),
						e);
			}

			long took = System.nanoTime() - start;
			String msg = "Initialized component {0}. Took {1}";
			logger.info(MessageFormat.format(msg, componentName, formatNanoDuration(took)));

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

			long start = System.nanoTime();

			StrolchComponent component = controller.getComponent();
			String componentName = component.getName();
			try {
				component.start();
			} catch (Exception e) {
				throw new StrolchException(MessageFormat.format("Failed to start component {0}", componentName), e);
			}

			long took = System.nanoTime() - start;
			String msg = "Started component {0}. Took {1}";
			logger.info(MessageFormat.format(msg, componentName, formatNanoDuration(took)));
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

			long start = System.nanoTime();

			StrolchComponent component = controller.getComponent();
			String componentName = component.getName();
			try {
				component.stop();
			} catch (Exception e) {
				String msg = "Failed to stop component {0} due to {1}";
				msg = MessageFormat.format(msg, componentName, e.getMessage());
				logger.error(msg, e);
			}

			long took = System.nanoTime() - start;
			String msg = "Stopped component {0}. Took {1}";
			logger.info(MessageFormat.format(msg, componentName, formatNanoDuration(took)));
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

			long start = System.nanoTime();

			StrolchComponent component = controller.getComponent();
			String componentName = component.getName();
			try {
				component.destroy();
			} catch (Exception e) {
				String msg = "Failed to destroy component {0} due to {1}";
				msg = MessageFormat.format(msg, componentName, e.getMessage());
				logger.error(msg, e);
			}

			long took = System.nanoTime() - start;
			String msg = "Destroyed component {0}. Took {1}";
			logger.info(MessageFormat.format(msg, componentName, formatNanoDuration(took)));
		}

		// Destroy direct upstream components
		Set<ComponentController> dependencies = this.dependencyAnalyzer.collectDirectUpstreamDependencies(controllers);
		if (!dependencies.isEmpty())
			destroy(dependencies);
	}
}
