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
package ch.eitchnet.xmlpers.test.impl;

import java.util.Properties;

import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory;
import ch.eitchnet.xmlpers.impl.XmlPersistenceFileDao;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TestModelDaoFactory implements XmlPersistenceDaoFactory {

	@Override
	public void initialize(XmlPersistenceFileDao fileDao, Properties properties) {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> XmlPersistenceDao<T> createDaoInstance(T object) {

		if (object.getClass() != Resource.class)
			throw new IllegalArgumentException("The object with class " + object.getClass() + " is not handled!");

		Resource resource = (Resource) object;
		String type = resource.getType();
		XmlPersistenceDao<Resource> dao;
		switch (type) {
		case "MyType":
			dao = new MyTypeResourceDao();
			break;
		default:
			throw new IllegalArgumentException("The resource with type " + type + " is not handled!");
		}

		// inject the DAO or SAX handler...

		@SuppressWarnings("unchecked")
		XmlPersistenceDao<T> xmlDao = (XmlPersistenceDao<T>) dao;
		return xmlDao;
	}

	@Override
	public <T> XmlPersistenceDao<T> createDaoInstance(String type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> XmlPersistenceDao<T> createDaoInstance(String type, String subType) {
		// TODO Auto-generated method stub
		return null;
	}

}
