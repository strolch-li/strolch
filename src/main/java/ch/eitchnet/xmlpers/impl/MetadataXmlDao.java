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

import java.util.Set;

import ch.eitchnet.xmlpers.api.XmlPersistenceFileHandler;
import ch.eitchnet.xmlpers.api.XmlPersistenceMetadataDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class MetadataXmlDao implements XmlPersistenceMetadataDao {

	private XmlPersistenceFileDao fileDao;
	private XmlPersistenceFileHandler fileHandler;

	// TODO think about setting some methods to final
	// TODO maybe decouple the write and load method into their own class

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
	public void removeAll() {
		this.fileDao.removeAll();
	}

	@Override
	public Set<String> queryKeySet(String type, String subType) {
		return this.fileDao.queryKeySet(type, subType);
	}

	@Override
	public Set<String> queryKeySet(String type) {
		return this.fileDao.queryKeySet(type);
	}

	@Override
	public Set<String> queryKeySet() {
		return this.fileDao.queryKeySet();
	}

	@Override
	public long querySize(String type, String subType) {
		return this.fileDao.querySize(type, subType);
	}

	@Override
	public long querySize(String type) {
		return this.fileDao.querySize(type);
	}

	@Override
	public long querySize() {
		return this.fileDao.querySize();
	}
}
