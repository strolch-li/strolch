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

import java.text.MessageFormat;

import li.strolch.runtime.configuration.ComponentConfiguration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class EmptyDataStoreModeAgentInitializer implements AgentLifecycleController {

	public static final String PROP_DATA_STORE_MODEL_FILE = "dataStoreModelFile"; //$NON-NLS-1$

	protected static final Logger logger = LoggerFactory.getLogger(EmptyDataStoreModeAgentInitializer.class);

	protected StrolchAgent strolchAgent;
	protected ComponentConfiguration configuration;
	protected OrderMap orderMap;
	protected ResourceMap resourceMap;

	/**
	 * @param strolchAgent
	 * @param configuration
	 */
	public EmptyDataStoreModeAgentInitializer(StrolchAgent strolchAgent, ComponentConfiguration configuration) {

		if (strolchAgent == null)
			throw new IllegalStateException("The StrolchAgent is not set!"); //$NON-NLS-1$
		if (configuration == null)
			throw new IllegalStateException("The ComponentConfiguration is not set!"); //$NON-NLS-1$
		if (!strolchAgent.getName().equals(configuration.getName())) {
			String msg = "The StrolchAgent and component configuration don't fit together as their names don't match: {0} / {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, strolchAgent.getName(), configuration.getName());
			throw new IllegalStateException(msg);
		}

		this.strolchAgent = strolchAgent;
		this.configuration = configuration;
	}

	@Override
	public void initialize() {
		this.resourceMap = new InMemoryResourceMap();
		this.orderMap = new InMemoryOrderMap();
	}

	@Override
	public void start() {
		this.strolchAgent.setResourceMap(this.resourceMap);
		this.strolchAgent.setOrderMap(this.orderMap);
	}

	@Override
	public void stop() {
		this.strolchAgent.setResourceMap(null);
		this.strolchAgent.setOrderMap(null);
	}

	@Override
	public void destroy() {
		this.strolchAgent.setResourceMap(null);
		this.strolchAgent.setOrderMap(null);
	}
}
