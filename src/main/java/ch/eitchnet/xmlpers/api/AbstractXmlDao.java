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
package ch.eitchnet.xmlpers.api;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.xmlpers.impl.XmlPersistenceFileDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractXmlDao<T> implements XmlPersistenceDao<T> {

	private XmlPersistenceFileDao fileDao;
	private XmlPersistenceFileHandler fileHandler;

	// TODO think about setting some methods to final
	// TODO if no sub type is given, then don't search recursively - it means subType does not exist

	void initialize(XmlPersistenceFileDao fileDao, XmlPersistenceFileHandler fileHandler) {
		if (fileDao == null || fileHandler == null)
			throw new IllegalArgumentException("Neither fileDao nor fileHandler may be null!");
		if (this.fileDao != null)
			throw new IllegalStateException("DAO is already initialized!");
		this.fileDao = fileDao;
		this.fileHandler = fileHandler;
	}

	protected XmlPersistenceFileDao getXmlPersistenceFileDao() {
		return this.fileDao;
	}

	protected XmlPersistenceFileHandler getXmlPersistenceFileHandler() {
		return this.fileHandler;
	}

	/**
	 * @return the fileHandler
	 */
	protected XmlPersistenceFileHandler getFileHandler() {
		return this.fileHandler;
	}

	/**
	 * @return the fileDao
	 */
	protected XmlPersistenceFileDao getFileDao() {
		return this.fileDao;
	}

	@Override
	public void remove(T object) {
		if (getSubType() == null)
			this.fileDao.remove(getType(), getId(object));
		else
			this.fileDao.remove(getType(), getSubType(), getId(object));
	}

	@Override
	public void removeById(String id) {
		if (getSubType() == null)
			this.fileDao.remove(getType(), id);
		else
			this.fileDao.remove(getType(), getSubType(), id);
	}

	@Override
	public void removeAll() {
		if (getSubType() == null)
			this.fileDao.removeAll(getType());
		else
			this.fileDao.removeAll(getType(), getSubType());
	}

	@Override
	public Set<String> queryKeySet() {
		if (getSubType() == null)
			return this.fileDao.queryKeySet(getType());
		return this.fileDao.queryKeySet(getType(), getSubType());
	}

	@Override
	public long querySize() {
		if (getSubType() == null)
			return this.fileDao.querySize(getType());
		return this.fileDao.querySize(getType(), getSubType());
	}

	@Override
	public List<T> queryAll() {

		if (getSubType() == null) {

			Set<String> idsByType = this.fileDao.queryKeySet(getType());
			List<T> result = new ArrayList<>(idsByType.size());

			for (String id : idsByType) {
				File objectPath = this.fileDao.getReadPath(getType(), id);
				String msg = "Can not read persistence units for {0} / {1} at {2}";
				assertReadable(objectPath, msg, getType(), id, objectPath);
				T object = read(objectPath);
				result.add(object);
			}

			return result;
		}

		Set<String> idsByType = this.fileDao.queryKeySet(getType(), getSubType());
		List<T> result = new ArrayList<>(idsByType.size());

		for (String id : idsByType) {
			File objectPath = this.fileDao.getReadPath(getType(), getSubType(), id);
			String msg = "Can not read persistence units for {0} / {1} / {2} at {3}";
			assertReadable(objectPath, msg, getType(), getSubType(), id, objectPath);
			T object = read(objectPath);
			result.add(object);
		}

		return result;
	}

	private void assertReadable(File persistenceUnit, String errorMsg, Object... msgArgs) {
		if (!persistenceUnit.canRead()) {
			String msg = MessageFormat.format(errorMsg, msgArgs);
			throw new XmlException(msg);
		}
	}

	@Override
	public T queryById(String id) {
		if (getSubType() == null) {
			File persistenceUnit = this.fileDao.getReadPath(getType(), id);
			if (!persistenceUnit.exists())
				return null;
			String msg = "Can not read persistence unit for {0} / {1} at {2}";
			assertReadable(persistenceUnit, msg, getType(), id, persistenceUnit);
			T object = read(persistenceUnit);
			return object;
		}

		File persistenceUnit = this.fileDao.getReadPath(getType(), getSubType(), id);
		if (!persistenceUnit.exists())
			return null;
		String msg = "Can not read persistence unit for {0} / {1} / {2} at {3}";
		assertReadable(persistenceUnit, msg, getType(), getSubType(), id, persistenceUnit);
		T object = read(persistenceUnit);
		return object;
	}

	@Override
	public void add(T object) {

		File addPath;
		if (getSubType() == null)
			addPath = this.fileDao.getAddPath(getType(), getId(object));
		else
			addPath = this.fileDao.getAddPath(getType(), getSubType(), getId(object));
		write(object, addPath);
	}

	@Override
	public void update(T object) {
		File addPath;
		if (getSubType() == null)
			addPath = this.fileDao.getUpdatePath(getType(), getId(object));
		else
			addPath = this.fileDao.getUpdatePath(getType(), getSubType(), getId(object));
		write(object, addPath);
	}

	/**
	 * Returns the type of domain object being handled by this {@link XmlPersistenceDao}. This would in most cases be
	 * the simple name of the class being persisted
	 * 
	 * @return the type of object being persisted
	 */
	protected abstract String getType();

	/**
	 * Returns the sub type, enabling categorizing types by a sub type. Default implementation returns null, thus no
	 * categorization is performed for this {@link XmlPersistenceDao} implementation
	 * 
	 * @return the sub type to further categorize the type of object being persisted
	 */
	protected String getSubType() {
		return null;
	}

	protected abstract String getId(T object);

	protected abstract T read(File filePath);

	protected abstract void write(T object, File filePath);
}
