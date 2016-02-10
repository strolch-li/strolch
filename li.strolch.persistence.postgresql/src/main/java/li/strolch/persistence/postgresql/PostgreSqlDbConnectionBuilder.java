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

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;
import java.util.logging.Logger;

import javax.sql.DataSource;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import ch.eitchnet.utils.dbc.DBC;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.DbConnectionBuilder;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public final class PostgreSqlDbConnectionBuilder extends DbConnectionBuilder {

	/**
	 * @param container
	 * @param persistenceHandlerConfiguration
	 */
	public PostgreSqlDbConnectionBuilder(ComponentContainer container,
			ComponentConfiguration persistenceHandlerConfiguration) {
		super(container, persistenceHandlerConfiguration);
	}

	@Override
	protected void validateConnection(DataSource dataSource) {
		super.validateConnection(dataSource);
		HikariDataSource ds = (HikariDataSource) dataSource;
		ds.validate();
	}

	@SuppressWarnings("resource")
	@Override
	public DataSource build(String realm, String url, String username, String password, Properties props) {

		HikariDataSource ds;
		HikariConfig config = new HikariConfig(props);
		config.setAutoCommit(false);
		config.setPoolName(realm);
		config.setJdbcUrl(url);
		config.setUsername(username);
		config.setPassword(password);

		ds = new HikariDataSource(config);
		logger.info("[" + realm + "] PostgreSQL Connection pool to " + url + " has a maximum pool size of "
				+ ds.getMaximumPoolSize() + " connections");

		return new StrolchPostgreDataSource(ds);
	}

	public class StrolchPostgreDataSource implements DataSource {

		private HikariDataSource ds;

		/**
		 * @param ds
		 */
		public StrolchPostgreDataSource(HikariDataSource ds) {
			DBC.PRE.assertNotNull("DataSource must be set!", ds);
			this.ds = ds;
		}

		/**
		 * @return
		 * @throws SQLException
		 * @see javax.sql.CommonDataSource#getLogWriter()
		 */
		@Override
		public PrintWriter getLogWriter() throws SQLException {
			return this.ds.getLogWriter();
		}

		/**
		 * @param iface
		 * @return
		 * @throws SQLException
		 * @see java.sql.Wrapper#unwrap(java.lang.Class)
		 */
		@Override
		public <T> T unwrap(Class<T> iface) throws SQLException {
			return this.ds.unwrap(iface);
		}

		/**
		 * @param out
		 * @throws SQLException
		 * @see javax.sql.CommonDataSource#setLogWriter(java.io.PrintWriter)
		 */
		@Override
		public void setLogWriter(PrintWriter out) throws SQLException {
			this.ds.setLogWriter(out);
		}

		/**
		 * @param iface
		 * @return
		 * @throws SQLException
		 * @see java.sql.Wrapper#isWrapperFor(java.lang.Class)
		 */
		@Override
		public boolean isWrapperFor(Class<?> iface) throws SQLException {
			return this.ds.isWrapperFor(iface);
		}

		/**
		 * @return
		 * @throws SQLException
		 * @see javax.sql.DataSource#getConnection()
		 */
		@Override
		public Connection getConnection() throws SQLException {
			return this.ds.getConnection();
		}

		/**
		 * @param seconds
		 * @throws SQLException
		 * @see javax.sql.CommonDataSource#setLoginTimeout(int)
		 */
		@Override
		public void setLoginTimeout(int seconds) throws SQLException {
			this.ds.setLoginTimeout(seconds);
		}

		/**
		 * @param username
		 * @param password
		 * @return
		 * @throws SQLException
		 * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
		 */
		@Override
		public Connection getConnection(String username, String password) throws SQLException {
			throw new UnsupportedOperationException("Deprecated, use parameterless version!");
		}

		/**
		 * @return
		 * @throws SQLException
		 * @see javax.sql.CommonDataSource#getLoginTimeout()
		 */
		@Override
		public int getLoginTimeout() throws SQLException {
			return this.ds.getLoginTimeout();
		}

		/**
		 * @return
		 * @throws SQLFeatureNotSupportedException
		 * @see javax.sql.CommonDataSource#getParentLogger()
		 */
		@Override
		public Logger getParentLogger() throws SQLFeatureNotSupportedException {
			return this.ds.getParentLogger();
		}

		@Override
		public String toString() {
			return "HikariDataSource for realm " + ds.getPoolName() + " for " + ds.getUsername() + " at "
					+ ds.getJdbcUrl();
		}

		/**
		 * @see com.zaxxer.hikari.HikariDataSource#shutdown()
		 */
		public void shutdown() {
			this.ds.shutdown();
		}

		/**
		 * @see com.zaxxer.hikari.AbstractHikariConfig#validate()
		 */
		public void validate() {
			this.ds.validate();
		}
	}
}