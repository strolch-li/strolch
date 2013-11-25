/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.agent;

import java.io.File;
import java.text.MessageFormat;
import java.util.List;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class StrolchAgent {

	public static final String PROP_DATA_STORE_MODE = "dataStoreMode"; //$NON-NLS-1$
	public static final String PROP_DATA_STORE_FILE = "dataStoreFile"; //$NON-NLS-1$
	private static final Logger logger = LoggerFactory.getLogger(StrolchAgent.class);

	private ComponentContainer container;
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
		ElementMapConfigurationCreator elementMapConfigurationCreator = dataStoreMode
				.getElementMapConfigurationConfigurator();
		List<ComponentConfiguration> componentConfigurations = elementMapConfigurationCreator
				.getComponentConfigurations(runtimeConfiguration);
		for (ComponentConfiguration configuration : componentConfigurations) {
			this.strolchConfiguration.addConfiguration(configuration.getName(), configuration);
		}
		
		ComponentContainer container = new ComponentContainer();
		this.container = container;

		logger.info(MessageFormat.format("Setup Agent {0}", runtimeConfiguration.getApplicationName())); //$NON-NLS-1$
	}
}
