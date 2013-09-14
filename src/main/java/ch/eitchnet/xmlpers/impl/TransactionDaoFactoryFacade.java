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
package ch.eitchnet.xmlpers.impl;

import java.util.Properties;

import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.api.XmlPersistenceMetadataDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TransactionDaoFactoryFacade implements XmlPersistenceDaoFactory {

	private XmlPersistenceDaoFactory daoFactory;
	private XmlPersistenceTransactionImpl tx;

	TransactionDaoFactoryFacade(XmlPersistenceDaoFactory daoFactory) {
		this.daoFactory = daoFactory;
	}

	void setTx(XmlPersistenceTransactionImpl tx) {
		this.tx = tx;
	}

	/**
	 * @throws UnsupportedOperationException
	 *             as this method may not be called on the facade
	 */
	@Override
	public void initialize(XmlPersistenceFileDao fileDao, Properties properties) throws UnsupportedOperationException {
		throw new UnsupportedOperationException();
	}

	/**
	 * @return
	 * @see ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory#getMetadataDao()
	 */
	@Override
	public XmlPersistenceMetadataDao getMetadataDao() {
		assertTxOpen();
		return this.daoFactory.getMetadataDao();
	}

	/**
	 * @param object
	 * @return
	 * @see ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory#getDao(java.lang.Object)
	 */
	@Override
	public <T> XmlPersistenceDao<T> getDao(T object) {
		assertTxOpen();
		return this.daoFactory.getDao(object);
	}

	/**
	 * @param type
	 * @return
	 * @see ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory#getDaoBy(java.lang.String)
	 */
	@Override
	public <T> XmlPersistenceDao<T> getDaoBy(String type) {
		assertTxOpen();
		return this.daoFactory.getDaoBy(type);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 * @see ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory#getDaoBy(java.lang.String, java.lang.String)
	 */
	@Override
	public <T> XmlPersistenceDao<T> getDaoBy(String type, String subType) {
		assertTxOpen();
		return this.daoFactory.getDaoBy(type, subType);
	}

	private void assertTxOpen() {
		if (this.tx.isCleared()) {
			throw new XmlPersistenceException(
					"The transaction has already been closed, thus no operation may be performed anymore with this dao factory instance");
		}
	}
}
