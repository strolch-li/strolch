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

import ch.eitchnet.xmlpers.api.XmlPersistenceMetadataDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class MetadataXmlDao implements XmlPersistenceMetadataDao {

	private XmlPersistenceFileDao fileDao;

	public void initialize(XmlPersistenceFileDao fileDao) {
		if (fileDao == null)
			throw new IllegalArgumentException("fileDao may not be null!");
		if (this.fileDao != null)
			throw new IllegalStateException("DAO is already initialized!");
		this.fileDao = fileDao;
	}

	/**
	 * @return the fileDao
	 */
	protected XmlPersistenceFileDao getFileDao() {
		return this.fileDao;
	}

	@Override
	public Set<String> queryTypeSet() {
		return this.fileDao.queryTypeSet();
	}

	@Override
	public Set<String> queryTypeSet(String type) {
		return this.fileDao.queryTypeSet(type);
	}

	@Override
	public Set<String> queryKeySet(String type) {
		return this.fileDao.queryKeySet(type);
	}

	@Override
	public Set<String> queryKeySet(String type, String subType) {
		return this.fileDao.queryKeySet(type, subType);
	}

	@Override
	public long queryTypeSize() {
		return this.fileDao.queryTypeSize();
	}

	@Override
	public long querySubTypeSize(String type) {
		return this.fileDao.queryTypeSize(type);
	}

	@Override
	public long querySize(String type) {
		return this.fileDao.querySize(type);
	}

	@Override
	public long querySize(String type, String subType) {
		return this.fileDao.querySize(type, subType);
	}
}
