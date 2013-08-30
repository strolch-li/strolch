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

import java.util.Properties;

import ch.eitchnet.xmlpers.impl.XmlPersistenceFileDao;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface XmlPersistenceDaoFactory {

	public void initialize(XmlPersistenceFileDao fileDao, Properties properties);

	public <T> XmlPersistenceMetadataDao getMetadataDao();

	public <T> XmlPersistenceDao<T> getDao(T object);

	public <T> XmlPersistenceDao<T> getDao(String type);

	public <T> XmlPersistenceDao<T> getDao(String type, String subType);
}
