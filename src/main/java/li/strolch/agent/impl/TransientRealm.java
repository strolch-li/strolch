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

import static ch.eitchnet.utils.helper.StringHelper.DOT;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.xml.XmlModelSaxFileReader;
import li.strolch.model.xml.XmlModelSaxReader.XmlModelStatistics;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.inmemory.InMemoryPersistence;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.StrolchConfigurationException;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class TransientRealm extends StrolchRealm {

	public static final String PREFIX_DATA_STORE_FILE = "dataStoreFile"; //$NON-NLS-1$

	private ResourceMap resourceMap;
	private OrderMap orderMap;
	private PersistenceHandler persistenceHandler;

	private File modelFile;

	public TransientRealm(String realm) {
		super(realm);
	}

	@Override
	public DataStoreMode getMode() {
		return DataStoreMode.TRANSIENT;
	}

	@Override
	public StrolchTransaction openTx() {
		return this.persistenceHandler.openTx(this);
	}

	@Override
	public ResourceMap getResourceMap() {
		return this.resourceMap;
	}

	@Override
	public OrderMap getOrderMap() {
		return this.orderMap;
	}

	@Override
	public void initialize(ComponentContainer container, ComponentConfiguration configuration) {
		super.initialize(container, configuration);

		String key = PREFIX_DATA_STORE_FILE;
		if (!getRealm().equals(StrolchConstants.DEFAULT_REALM))
			key += DOT + getRealm();

		if (!configuration.hasProperty(key)) {
			String msg = "There is no data store file for realm {0}. Set a property with key {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, getRealm(), key);
			throw new StrolchConfigurationException(msg);
		}

		this.modelFile = configuration.getDataFile(key, null, configuration.getRuntimeConfiguration(), true);

		this.persistenceHandler = new InMemoryPersistence();
		this.resourceMap = new TransactionalResourceMap();
		this.orderMap = new TransactionalOrderMap();
	}

	@Override
	public void start() {

		XmlModelStatistics statistics;
		try (StrolchTransaction tx = openTx()) {
			InMemoryElementListener elementListener = new InMemoryElementListener(tx);
			XmlModelSaxFileReader handler = new XmlModelSaxFileReader(elementListener, modelFile);
			handler.parseFile();
			statistics = handler.getStatistics();
		}

		String durationS = StringHelper.formatNanoDuration(statistics.durationNanos);
		logger.info(MessageFormat.format(
				"Loading XML Model file {0} for realm {1} took {2}.", modelFile.getName(), getRealm(), durationS)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Orders", statistics.nrOfOrders)); //$NON-NLS-1$
		logger.info(MessageFormat.format("Loaded {0} Resources", statistics.nrOfResources)); //$NON-NLS-1$
	}

	@Override
	public void stop() {
		// 
	}

	@Override
	public void destroy() {
		// 
	}
}
