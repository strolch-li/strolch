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

import li.strolch.model.xml.XmlModelDefaultHandler.XmlModelStatistics;
import li.strolch.model.xml.XmlModelFileHandler;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransientElementMapHandler extends InMemoryElementMapHandler {

	private File modelFile;

	/**
	 * @param container
	 * @param componentName
	 */
	public TransientElementMapHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {

		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		File modelFile = runtimeConfiguration.getDataFile(StrolchAgent.PROP_DATA_STORE_FILE, null,
				runtimeConfiguration, true);
		this.modelFile = modelFile;

		this.resourceMap = new InMemoryResourceMap();
		this.orderMap = new InMemoryOrderMap();

		super.initialize(configuration);
	}

	@Override
	public void start() {

		InMemoryElementListener elementListener = new InMemoryElementListener(this.resourceMap, this.orderMap);
		XmlModelFileHandler handler = new XmlModelFileHandler(elementListener, this.modelFile);
		handler.parseFile();
		XmlModelStatistics statistics = handler.getStatistics();
		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info(MessageFormat.format("Loading XML Model file {0} took {1}.", this.modelFile.getName(), durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$

		super.start();
	}
}
