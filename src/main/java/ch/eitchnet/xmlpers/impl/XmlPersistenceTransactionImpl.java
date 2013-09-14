/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.xmlpers
 *
 * ch.eitchnet.java.xmlpers is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.xmlpers is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.xmlpers.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.xmlpers.impl;

import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.objectfilter.ObjectFilter;
import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory;
import ch.eitchnet.xmlpers.api.XmlPersistenceTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceTransactionImpl implements XmlPersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceTransactionImpl.class);

	private static final ThreadLocal<XmlPersistenceTransactionImpl> TX_THREADLOCAL_THREAD_LOCAL;
	static {
		TX_THREADLOCAL_THREAD_LOCAL = new ThreadLocal<>();
	}

	private XmlPersistenceDaoFactory daoFactory;
	private ObjectFilter objectFilter;
	private boolean verbose;
	private boolean cleared;

	/**
	 * @param verbose
	 */
	public XmlPersistenceTransactionImpl(XmlPersistenceDaoFactory daoFactory, boolean verbose) {
		this.daoFactory = daoFactory;
		this.verbose = verbose;
		this.objectFilter = new ObjectFilter();
	}

	/*
	 * modifying methods
	 */

	/**
	 * @param object
	 */
	@Override
	public <T> void add(T object) {
		this.objectFilter.add(object);
	}

	/**
	 * @param objects
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T> void addAll(List<T> objects) {
		this.objectFilter.addAll((List<Object>) objects);
	}

	/**
	 * @param object
	 */
	@Override
	public <T> void update(T object) {
		this.objectFilter.update(object);
	}

	/**
	 * @param objects
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T> void updateAll(List<T> objects) {
		this.objectFilter.updateAll((List<Object>) objects);
	}

	/**
	 * @param object
	 */
	@Override
	public <T> void remove(T object) {
		this.objectFilter.remove(object);
	}

	/**
	 * @param objects
	 */
	@Override
	@SuppressWarnings("unchecked")
	public <T> void removeAll(List<T> objects) {
		this.objectFilter.removeAll((List<Object>) objects);
	}

	/**
	 * @return the daoFactory
	 */
	@Override
	public XmlPersistenceDaoFactory getDaoFactory() {
		return this.daoFactory;
	}

	/**
	 * 
	 */
	@Override
	public void commit() {

		try {
			if (this.verbose)
				XmlPersistenceTransactionImpl.logger.info("Committing TX...");
			Set<String> keySet = this.objectFilter.keySet();
			if (keySet.isEmpty())
				return;
			for (String key : keySet) {

				List<Object> removed = this.objectFilter.getRemoved(key);
				if (removed.isEmpty()) {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info("No objects removed in this tx.");
				} else {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info(removed.size() + " objects removed in this tx.");

					for (Object object : removed) {
						XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
						dao.remove(object);
					}
				}

				List<Object> updated = this.objectFilter.getUpdated(key);
				if (updated.isEmpty()) {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info("No objects updated in this tx.");
				} else {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info(updated.size() + " objects updated in this tx.");

					for (Object object : updated) {

						XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
						dao.update(object);
					}
				}

				List<Object> added = this.objectFilter.getAdded(key);
				if (added.isEmpty()) {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info("No objects added in this tx.");
				} else {
					if (this.verbose)
						XmlPersistenceTransactionImpl.logger.info(added.size() + " objects added in this tx.");

					for (Object object : added) {

						XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
						dao.add(object);
					}
				}
			}

			XmlPersistenceTransactionImpl.logger.info("Completed TX");

		} finally {
			// clean up
			clear();
		}
	}

	/**
	 * Clears the object filter and releases the transaction. After calling this method, this transaction instance can
	 * not be used anymore
	 */
	@Override
	public void clear() {
		if (!this.cleared) {
			this.objectFilter.clearCache();
			this.objectFilter = null;

			this.daoFactory = null;
			TX_THREADLOCAL_THREAD_LOCAL.set(null);
			this.cleared = true;
		}
	}

	/**
	 * @return
	 */
	public boolean isCleared() {
		return this.cleared;
	}

	public static XmlPersistenceTransaction getTx() {
		XmlPersistenceTransaction tx = TX_THREADLOCAL_THREAD_LOCAL.get();
		if (tx == null)
			throw new IllegalStateException("No transaction is currently open!");
		return tx;
	}

	public static void setTx(XmlPersistenceTransactionImpl tx) {
		if (TX_THREADLOCAL_THREAD_LOCAL.get() != null)
			throw new IllegalStateException("A transaction is already open!");
		TX_THREADLOCAL_THREAD_LOCAL.set(tx);
	}
}
