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
package li.strolch.persistence.impl.dao.test;

import java.io.File;
import java.text.MessageFormat;

import li.strolch.persistence.impl.XmlPersistenceHandler;
import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.ConfigurationParser;
import li.strolch.runtime.configuration.StrolchConfiguration;
import li.strolch.testbase.runtime.RuntimeMock;

import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractDaoImplTest {

	private static final String RUNTIME_PATH = "target/strolchRuntime/"; //$NON-NLS-1$
	private static final String CONFIG_SRC = "src/test/resources/runtime/config"; //$NON-NLS-1$
	protected static final Logger logger = LoggerFactory.getLogger(AbstractDaoImplTest.class);

	protected static XmlPersistenceHandler persistenceHandler;

	@BeforeClass
	public static void beforeClass() {

		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		RuntimeMock.mockRuntime(rootPath, configSrc);

		File dbStorePath = new File(rootPath, XmlPersistenceHandler.DB_STORE_PATH);
		if (!dbStorePath.mkdirs()) {
			String msg = "Failed to create path {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, dbStorePath.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		// initialize the component configuration
		StrolchConfiguration strolchConfiguration = ConfigurationParser.parseConfiguration(rootPath);
		ComponentConfiguration componentConfiguration = strolchConfiguration
				.getComponentConfiguration("PersistenceHandler"); //$NON-NLS-1$
		ComponentContainer container = new ComponentContainer();
		persistenceHandler = new XmlPersistenceHandler(container);
		persistenceHandler.initialize(componentConfiguration);

	}
}
