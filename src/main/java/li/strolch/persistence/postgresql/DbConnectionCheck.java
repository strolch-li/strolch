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
package li.strolch.persistence.postgresql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.persistence.api.DbConnectionInfo;
import li.strolch.runtime.configuration.StrolchConfigurationException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public class DbConnectionCheck {

	private static final Logger logger = LoggerFactory.getLogger(DbConnectionCheck.class);
	private Map<String, DbConnectionInfo> connetionInfoMap;

	/**
	 * @param connetionInfoMap
	 */
	public DbConnectionCheck(Map<String, DbConnectionInfo> connetionInfoMap) {
		this.connetionInfoMap = connetionInfoMap;
	}

	public void checkConnections() {
		Collection<DbConnectionInfo> values = this.connetionInfoMap.values();
		for (DbConnectionInfo connectionInfo : values) {
			String url = connectionInfo.getUrl();
			String username = connectionInfo.getUsername();
			String password = connectionInfo.getPassword();

			try (Connection con = DriverManager.getConnection(url, username, password);
					Statement st = con.createStatement();) {

				try (ResultSet rs = st.executeQuery("SELECT VERSION()")) {
					if (rs.next()) {
						logger.info("Connected to: " + rs.getString(1));
					}
				}

			} catch (SQLException e) {
				String msg = "Failed to open DB connection to URL {0} due to: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, url, e.getMessage());
				throw new StrolchConfigurationException(msg, e);
			}
		}
	}

}
