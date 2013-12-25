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
package li.strolch.runtime.agent.impl;

import java.io.File;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Map;

import li.strolch.model.xml.XmlModelDefaultHandler.XmlModelStatistics;
import li.strolch.model.xml.XmlModelFileHandler;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.agent.api.OrderMap;
import li.strolch.runtime.agent.api.ResourceMap;
import li.strolch.runtime.agent.api.StrolchAgent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransientElementMapHandler extends InMemoryElementMapHandler {

	private Map<String, File> realmModelFiles;

	/**
	 * @param container
	 * @param componentName
	 */
	public TransientElementMapHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);

		this.realmModelFiles = new HashMap<>();
		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		for (String realm : this.realms.keySet()) {
			String key = getDataStoreFilePropKey(realm);

			if (!runtimeConfiguration.hasProperty(key)) {
				String msg = "There is no data store file for realm {0}. Set a property with key {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, realm, key);
				throw new StrolchConfigurationException(msg);
			}

			File modelFile = runtimeConfiguration.getDataFile(key, null, runtimeConfiguration, true);
			this.realmModelFiles.put(realm, modelFile);
		}
	}

	private String getDataStoreFilePropKey(String realm) {
		if (realm.equals(StrolchConstants.DEFAULT_REALM))
			return StrolchAgent.PROP_DATA_STORE_FILE;
		return StrolchAgent.PROP_DATA_STORE_FILE + "." + realm; //$NON-NLS-1$
	}

	@Override
	public void start() {

		for (String realm : this.realms.keySet()) {

			StrolchRealm strolchRealm = this.realms.get(realm);
			ResourceMap resourceMap = strolchRealm.getResourceMap();
			OrderMap orderMap = strolchRealm.getOrderMap();

			File modelFile = this.realmModelFiles.get(realm);
			InMemoryElementListener elementListener = new InMemoryElementListener(resourceMap, orderMap);
			XmlModelFileHandler handler = new XmlModelFileHandler(elementListener, modelFile);
			handler.parseFile();
			XmlModelStatistics statistics = handler.getStatistics();
			String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
			logger.info(MessageFormat.format(
					"Loading XML Model file {0} for realm {1} took {2}.", modelFile.getName(), realm, durationS)); //$NON-NLS-1$
			logger.info(MessageFormat.format("Loaded {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
			logger.info(MessageFormat.format("Loaded {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$
		}

		super.start();
	}
}
