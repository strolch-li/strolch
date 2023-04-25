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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.*;
import li.strolch.privilege.model.Certificate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PostgreSqlStrolchTransaction extends AbstractTransaction {

	private static final Logger logger = LoggerFactory.getLogger(PostgreSqlStrolchTransaction.class);
	private final PostgreSqlPersistenceHandler persistenceHandler;

	private PostgreSqlOrderDao orderDao;
	private PostgreSqlResourceDao resourceDao;
	private PostgreSqlActivityDao activityDao;
	private PostgreSqlLogMessageDao logMessageDao;
	private AuditDao auditDao;
	private Connection connection;

	public PostgreSqlStrolchTransaction(ComponentContainer container, StrolchRealm realm, Certificate certificate,
			String action, boolean readOnly, PostgreSqlPersistenceHandler persistenceHandler) {
		super(container, realm, certificate, action, readOnly);
		this.persistenceHandler = persistenceHandler;
	}

	@Override
	protected void writeChanges() {

		// first perform DAOs
		if (this.orderDao != null)
			this.orderDao.flush();
		if (this.resourceDao != null)
			this.resourceDao.flush();
		if (this.activityDao != null)
			this.activityDao.flush();

		// don't commit the connection, this is done in postCommit when we close the connection
	}

	@Override
	protected void rollback() throws Exception {
		if (this.connection != null) {
			try {
				this.connection.rollback();
			} finally {
				try {
					this.connection.close();
				} catch (Exception e) {
					logger.error("Failed to close connection due to " + e.getMessage(), e);
				}
			}
		}
	}

	@Override
	protected void commit() throws Exception {
		if (this.connection != null) {
			this.connection.commit();
			this.connection.close();
		}
	}

	OrderDao getOrderDao() {
		if (this.orderDao == null)
			this.orderDao = new PostgreSqlOrderDao(this.persistenceHandler.getDataType(), getConnection(),
					getTxResult(), isVersioningEnabled());
		return this.orderDao;
	}

	ResourceDao getResourceDao() {
		if (this.resourceDao == null)
			this.resourceDao = new PostgreSqlResourceDao(this.persistenceHandler.getDataType(), getConnection(),
					getTxResult(), isVersioningEnabled());
		return this.resourceDao;
	}

	ActivityDao getActivityDao() {
		if (this.activityDao == null)
			this.activityDao = new PostgreSqlActivityDao(this.persistenceHandler.getDataType(), getConnection(),
					getTxResult(), isVersioningEnabled());
		return this.activityDao;
	}

	AuditDao getAuditDao() {
		if (this.auditDao == null)
			this.auditDao = new PostgreSqlAuditDao(this);
		return this.auditDao;
	}

	LogMessageDao getLogMessageDao() {
		if (this.logMessageDao == null)
			this.logMessageDao = new PostgreSqlLogMessageDao(this);
		return this.logMessageDao;
	}

	Connection getConnection() {
		if (this.connection == null)
			this.connection = this.persistenceHandler.getConnection(getRealm().getRealm());
		return this.connection;
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
