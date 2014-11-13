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

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.text.MessageFormat;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DbConnectionInfo {

	private String realm;
	private String url;
	private String username;
	private String password;

	public DbConnectionInfo(String realm, String url) {
		DBC.PRE.assertNotEmpty("Realm must be set!", realm); //$NON-NLS-1$
		DBC.PRE.assertNotEmpty("Url must be set!", url); //$NON-NLS-1$
		this.realm = realm;
		this.url = url;
	}

	/**
	 * @return the realm
	 */
	public String getRealm() {
		return this.realm;
	}

	/**
	 * @param realm
	 *            the realm to set
	 */
	public void setRealm(String realm) {
		this.realm = realm;
	}

	/**
	 * @return the url
	 */
	public String getUrl() {
		return this.url;
	}

	/**
	 * @param url
	 *            the url to set
	 */
	public void setUrl(String url) {
		this.url = url;
	}

	/**
	 * @return the username
	 */
	public String getUsername() {
		return this.username;
	}

	/**
	 * @param username
	 *            the username to set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return this.password;
	}

	/**
	 * @param password
	 *            the password to set
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	@SuppressWarnings("nls")
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("DbConnectionInfo [realm=");
		builder.append(this.realm);
		builder.append(", url=");
		builder.append(this.url);
		builder.append(", username=");
		builder.append(this.username);
		builder.append(", password=***");
		builder.append("]");
		return builder.toString();
	}

	/**
	 * @return a {@link Connection}
	 * 
	 * @throws DbException
	 */
	public Connection openConnection() throws DbException {
		try {
			Connection connection = DriverManager.getConnection(this.url, this.username, this.password);
			connection.setAutoCommit(false);
			return connection;
		} catch (SQLException e) {
			String msg = MessageFormat.format("Failed to get a connection for {0} due to {1}", this, e.getMessage()); //$NON-NLS-1$
			throw new DbException(msg, e);
		}
	}
}
