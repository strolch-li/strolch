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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import ch.eitchnet.utils.exceptions.XmlException;
import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceFileHandler;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class AbstractXmlDao<T> implements XmlPersistenceDao<T> {

	private XmlPersistenceFileDao fileDao;
	private XmlPersistenceFileHandler fileHandler;

	// TODO think about setting some methods to final
	// TODO maybe decouple the write and load method into their own class
	// TODO if no sub type is given, then don't search recursively - it means subType does not exist

	@Override
	public void setXmlPersistenceFileDao(XmlPersistenceFileDao fileDao) {
		this.fileDao = fileDao;
	}

	@Override
	public void setXmlPersistenceFileHandler(XmlPersistenceFileHandler fileHandler) {
		this.fileHandler = fileHandler;
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
		this.fileDao.remove(getType(), getSubType(), getId(object));
	}

	@Override
	public void removeById(String id) {
		this.fileDao.remove(getType(), getSubType(), id);
	}

	@Override
	public void removeAll() {
		this.fileDao.removeAll(getType(), getSubType());
	}

	@Override
	public Set<String> queryKeySet() {
		return this.fileDao.queryKeySet(getType(), getSubType());
	}

	@Override
	public long querySize() {
		return this.fileDao.querySize(getType(), getSubType());
	}

	@Override
	public List<T> queryAll() {
		File queryPath = this.fileDao.getPathBuilder().getPath(getType(), getSubType());
		String msg = "Can not read persistence unit for {0} / {1} at {2}";
		return queryAll(queryPath, msg, getType(), getSubType(), queryPath);
	}

	private List<T> queryAll(File queryPath, String errorMsg, Object... msgArgs) {
		File[] persistenceUnits = queryPath.listFiles();
		List<T> result = new ArrayList<>();
		for (File persistenceUnit : persistenceUnits) {
			if (!persistenceUnit.isFile())
				continue;

			assertReadable(persistenceUnit, errorMsg, msgArgs);
			T object = read(persistenceUnit);
			result.add(object);
		}

		return result;
	}

	private void assertReadable(File persistenceUnit, String errorMsg, Object... msgArgs) {
		if (!persistenceUnit.canRead()) {
			String msg = String.format(errorMsg, msgArgs);
			throw new XmlException(msg);
		}
	}

	@Override
	public T queryById(String id) {
		File persistenceUnit = this.fileDao.getPathBuilder().getPath(getType(), getSubType(), id);
		String msg = "Can not read persistence unit for {0} / {1} / {2} at {3}";
		return queryById(persistenceUnit, msg, getType(), getSubType(), id, persistenceUnit);
	}

	private T queryById(File persistenceUnit, String msg, Object... msgArgs) {
		assertReadable(persistenceUnit, msg, msgArgs);
		T object = read(persistenceUnit);
		return object;
	}

	@Override
	public void add(T object) {
		XmlPersistencePathBuilder pathBuilder = this.fileDao.getPathBuilder();
		File addPath = pathBuilder.getAddPath(getType(), getSubType(), getId(object));
		write(object, addPath);
	}

	@Override
	public void update(T object) {
		XmlPersistencePathBuilder pathBuilder = this.fileDao.getPathBuilder();
		File updatePath = pathBuilder.getUpdatePath(getType(), getSubType(), getId(object));
		write(object, updatePath);
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
