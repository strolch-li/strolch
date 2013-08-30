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
package ch.eitchnet.xmlpers.api;

import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.objectfilter.ObjectFilter;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceTransaction {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceTransaction.class);

	private static final ThreadLocal<XmlPersistenceTransaction> TX_THREADLOCAL_THREAD_LOCAL;
	static {
		TX_THREADLOCAL_THREAD_LOCAL = new ThreadLocal<>();
	}

	private final XmlPersistenceDaoFactory daoFactory;
	private final boolean verbose;
	private final ObjectFilter objectFilter;

	/**
	 * @param verbose
	 */
	public XmlPersistenceTransaction(XmlPersistenceDaoFactory daoFactory, boolean verbose) {
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
	public <T> void add(T object) {
		this.objectFilter.add(object);
	}

	/**
	 * @param objects
	 */
	@SuppressWarnings("unchecked")
	public <T> void addAll(List<T> objects) {
		this.objectFilter.addAll((List<Object>) objects);
	}

	/**
	 * @param object
	 */
	public <T> void update(T object) {
		this.objectFilter.update(object);
	}

	/**
	 * @param objects
	 */
	@SuppressWarnings("unchecked")
	public <T> void updateAll(List<T> objects) {
		this.objectFilter.updateAll((List<Object>) objects);
	}

	/**
	 * @param object
	 */
	public <T> void remove(T object) {
		this.objectFilter.remove(object);
	}

	/**
	 * @param objects
	 */
	@SuppressWarnings("unchecked")
	public <T> void removeAll(List<T> objects) {
		this.objectFilter.removeAll((List<Object>) objects);
	}

	/*
	 * querying methods
	 */

	/**
	 * @param type
	 * @return
	 */
	public Set<String> queryKeySet() {
		return this.daoFactory.getMetadataDao().queryKeySet();
	}

	/**
	 * @param type
	 * @return
	 */
	public Set<String> queryKeySet(String type) {
		return this.daoFactory.getMetadataDao().queryKeySet(type);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public Set<String> queryKeySet(String type, String subType) {
		return this.daoFactory.getMetadataDao().queryKeySet(type, subType);
	}

	/**
	 * @param type
	 * @return
	 */
	public long querySize(String type) {
		return querySize(type, null);
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public long querySize(String type, String subType) {
		return this.daoFactory.getDao(type, subType).querySize();
	}

	/**
	 * @param type
	 * @return
	 */
	public <T> List<T> queryAll(String type) {
		XmlPersistenceDao<T> dao = this.daoFactory.getDao(type);
		List<T> objects = dao.queryAll(type);
		return objects;
	}

	/**
	 * @param type
	 * @param subType
	 * @return
	 */
	public <T> List<T> queryAll(String type, String subType) {
		XmlPersistenceDao<T> dao = this.daoFactory.getDao(type, subType);
		List<T> objects = dao.queryAll(type, subType);
		return objects;
	}

	/**
	 * @param type
	 * @param id
	 * @return
	 */
	public <T> T queryById(String type, String id) {
		return queryById(type, id);
	}

	/**
	 * @param type
	 * @param subType
	 * @param id
	 * @return
	 */
	public <T> T queryById(String type, String subType, String id) {
		XmlPersistenceDao<T> dao = this.daoFactory.getDao(type, subType);
		T object = dao.queryById(id);
		return object;
	}

	/**
	 * 
	 */
	public void commit() {

		if (this.verbose)
			XmlPersistenceTransaction.logger.info("Committing TX...");

		Set<String> keySet = this.objectFilter.keySet();
		if (keySet.isEmpty())
			return;

		for (String key : keySet) {

			List<Object> removed = this.objectFilter.getRemoved(key);
			if (removed.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects removed in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(removed.size() + " objects removed in this tx.");

				for (Object object : removed) {
					XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
					dao.remove(object);
				}
			}

			List<Object> updated = this.objectFilter.getUpdated(key);
			if (updated.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects updated in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(updated.size() + " objects updated in this tx.");

				for (Object object : updated) {

					XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
					dao.update(object);
				}
			}

			List<Object> added = this.objectFilter.getAdded(key);
			if (added.isEmpty()) {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info("No objects added in this tx.");
			} else {
				if (this.verbose)
					XmlPersistenceTransaction.logger.info(added.size() + " objects added in this tx.");

				for (Object object : added) {

					XmlPersistenceDao<Object> dao = this.daoFactory.getDao(object);
					dao.add(object);
				}
			}
		}

		this.objectFilter.clearCache();
		XmlPersistenceTransaction.logger.info("Completed TX");
	}

	public static XmlPersistenceTransaction getTx() {
		XmlPersistenceTransaction tx = TX_THREADLOCAL_THREAD_LOCAL.get();
		if (tx == null)
			throw new IllegalStateException("No transaction is currently open!");
		return tx;
	}

	public static void setTx(XmlPersistenceTransaction tx) {
		if (TX_THREADLOCAL_THREAD_LOCAL.get() != null)
			throw new IllegalStateException("A transaction is already open!");
		TX_THREADLOCAL_THREAD_LOCAL.set(tx);
	}
}
