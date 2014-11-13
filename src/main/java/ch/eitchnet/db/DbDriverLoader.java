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
package ch.eitchnet.db;

import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbDriverLoader {

	private static final Logger logger = LoggerFactory.getLogger(DbDriverLoader.class);

	public static void loadDriverForConnection(DbConnectionInfo connectionInfo) throws DbException {
		Driver driver;
		try {
			driver = DriverManager.getDriver(connectionInfo.getUrl());
		} catch (SQLException e) {
			String msg = "Failed to load DB driver for URL {0} due to: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, connectionInfo.getUrl(), e.getMessage());
			throw new DbException(msg, e);
		}

		String compliant = driver.jdbcCompliant() ? "" : "non"; //$NON-NLS-1$ //$NON-NLS-2$
		String msg = "Realm {0}: Using {1} JDBC compliant Driver {2}.{3}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, connectionInfo.getRealm(), compliant, driver.getMajorVersion(),
				driver.getMinorVersion());
		logger.info(msg);
	}
}
