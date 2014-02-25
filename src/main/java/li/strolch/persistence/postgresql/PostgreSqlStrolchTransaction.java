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

import li.strolch.agent.api.StrolchRealm;
import li.strolch.persistence.api.AbstractTransaction;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.TransactionResult;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PostgreSqlStrolchTransaction extends AbstractTransaction {

	private static final Logger logger = LoggerFactory.getLogger(PostgreSqlStrolchTransaction.class);
	private PostgreSqlPersistenceHandler persistenceHandler;

	private PostgresqlDao<?> orderDao;
	private PostgresqlDao<?> resourceDao;
	private Connection connection;

	public PostgreSqlStrolchTransaction(StrolchRealm realm, PostgreSqlPersistenceHandler persistenceHandler) {
		super(realm);
		this.persistenceHandler = persistenceHandler;
	}

	@Override
	protected void commit(TransactionResult txResult) throws Exception {

		// first perform DAOs
		if (this.orderDao != null)
			this.orderDao.commit(txResult);
		if (this.resourceDao != null)
			this.resourceDao.commit(txResult);

		// then commit the SQL connection
		if (this.connection != null)
			this.connection.commit();

		// and close the connection, but not catching, as otherwise we can't rollback in exception case
		this.connection.close();
	}

	@Override
	protected void rollback(TransactionResult txResult) throws Exception {
		if (this.connection != null) {
			try {
				this.connection.rollback();
			} finally {
				try {
					this.connection.close();
				} catch (Exception e) {
					logger.error("Failed to close connection due to " + e.getMessage(), e); //$NON-NLS-1$
				}
			}
		}
	}

	OrderDao getOrderDao() {
		if (this.orderDao == null)
			this.orderDao = new PostgreSqlOrderDao(this);
		return (OrderDao) this.orderDao;
	}

	ResourceDao getResourceDao() {
		if (this.resourceDao == null)
			this.resourceDao = new PostgreSqlResourceDao(this);
		return (ResourceDao) this.resourceDao;
	}

	Connection getConnection() {
		if (this.connection == null) {
			this.connection = this.persistenceHandler.getConnection(getRealm().getRealm());
		}
		return this.connection;
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}
}
