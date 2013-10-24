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
import java.util.HashMap;
import java.util.Map;

import li.strolch.persistence.api.StrolchPersistenceHandler;
import li.strolch.persistence.impl.XmlPersistenceHandler;
import li.strolch.runtime.ComponentConfiguration;
import li.strolch.runtime.RuntimeConfiguration;

import org.junit.BeforeClass;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractDaoImplTest {

	private static final String RUNTIME_PATH = "target/strolchRuntime/"; //$NON-NLS-1$
	protected static final Logger logger = LoggerFactory.getLogger(AbstractDaoImplTest.class);

	protected static XmlPersistenceHandler persistenceHandler;

	@BeforeClass
	public static void beforeClass() {

		File file = new File(RUNTIME_PATH);
		if (file.exists()) {
			if (!FileHelper.deleteFile(file, true)) {
				String msg = "Failed to delete {0}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, file.getAbsolutePath());
				throw new RuntimeException(msg);
			}
		}

		file = new File(file, XmlPersistenceHandler.DB_STORE_PATH);
		if (!file.mkdirs()) {
			String msg = "Failed to create path {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, file.getAbsolutePath());
			throw new RuntimeException(msg);
		}

		// initialize a runtime configuration
		Map<String, String> runtimeConfigurationValues = new HashMap<>();
		String rootPath = RUNTIME_PATH;
		RuntimeConfiguration runtimeConfiguration = new RuntimeConfiguration(runtimeConfigurationValues, rootPath);

		// initialize the component configuration
		String componentName = StrolchPersistenceHandler.class.getName();
		Map<String, String> configurationValues = new HashMap<>();
		ComponentConfiguration componentConfiguration = new ComponentConfiguration(runtimeConfiguration, componentName,
				configurationValues);

		persistenceHandler = new XmlPersistenceHandler();
		persistenceHandler.initialize(componentConfiguration);

	}

}
