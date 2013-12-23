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
package li.strolch.runtime.agent;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StrolchAgent {

	public static final String PROP_DATA_STORE_MODE = "dataStoreMode"; //$NON-NLS-1$
	public static final String PROP_DATA_STORE_FILE = "dataStoreFile"; //$NON-NLS-1$
	public static final String PROP_REALMS = "realms"; //$NON-NLS-1$
	private static final Logger logger = LoggerFactory.getLogger(StrolchAgent.class);

	private ComponentContainerImpl container;
	private StrolchConfiguration strolchConfiguration;

	/**
	 * @return the strolchConfiguration
	 */
	public StrolchConfiguration getStrolchConfiguration() {
		return this.strolchConfiguration;
	}

	/**
	 * @return the container
	 */
	public ComponentContainer getContainer() {
		return this.container;
	}

	public String getApplicationName() {
		return this.strolchConfiguration.getRuntimeConfiguration().getApplicationName();
	}

	public void initialize() {
		this.container.initialize(this.strolchConfiguration);
	}

	public void start() {
		this.container.start();
	}

	public void stop() {
		this.container.stop();
	}

	public void destroy() {
		this.container.destroy();
	}

	public void setup(File path) {

		String msg = "Setting up Strolch Container from root {0}"; //$NON-NLS-1$
		logger.info(MessageFormat.format(msg, path.getAbsolutePath()));

		this.strolchConfiguration = ConfigurationParser.parseConfiguration(path);
		RuntimeConfiguration runtimeConfiguration = this.strolchConfiguration.getRuntimeConfiguration();
		DataStoreMode dataStoreMode = DataStoreMode.parseDataStoreMode(runtimeConfiguration.getString(
				PROP_DATA_STORE_MODE, null));

		ElementMapHandlerConfigurator mapHandlerConfigurator = dataStoreMode.getElementMapConfigurationConfigurator();
		ComponentConfiguration configuration = mapHandlerConfigurator.buildConfiguration(this);
		this.strolchConfiguration.addConfiguration(configuration.getName(), configuration);

		ComponentContainerImpl container = new ComponentContainerImpl();
		this.container = container;

		logger.info(MessageFormat.format("Setup Agent {0}", runtimeConfiguration.getApplicationName())); //$NON-NLS-1$
	}

	public ResourceMap getResourceMap() {
		assertContainerStarted();
		return getContainer().getComponent(ElementMapHandler.class).getResourceMap();
	}

	public OrderMap getOrderMap() {
		assertContainerStarted();
		return getContainer().getComponent(ElementMapHandler.class).getOrderMap();
	}

	protected void assertContainerStarted() {
		if (this.container == null || this.container.getState() != ComponentState.STARTED) {
			String msg = "Container is not yet started!"; //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}
	}
}
