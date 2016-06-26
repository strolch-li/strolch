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
package li.strolch.db;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbConnectionCheck {

	private static final Logger logger = LoggerFactory.getLogger(DbConnectionCheck.class);
	private Map<String, DataSource> dsMap;

	/**
	 * @param dsMap
	 */
	public DbConnectionCheck(Map<String, DataSource> dsMap) {
		this.dsMap = dsMap;
	}

	/**
	 * Checks the connectivity to each of the configured {@link DbConnectionInfo}
	 * 
	 * @throws DbException
	 */
	public void checkConnections() throws DbException {
		Collection<DataSource> values = this.dsMap.values();
		for (DataSource ds : values) {

			logger.info("Checking connection " + ds);

			try (Connection con = ds.getConnection(); Statement st = con.createStatement();) {

				try (ResultSet rs = st.executeQuery("select version()")) { //$NON-NLS-1$
					if (rs.next()) {
						logger.info(MessageFormat.format("Connected to: {0}", rs.getString(1))); //$NON-NLS-1$
					}
				}

			} catch (SQLException e) {
				String msg = "Failed to open DB connection to {0} due to: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, ds, e.getMessage());
				throw new DbException(msg, e);
			}
		}

		logger.info("All connections OK");
	}
}
