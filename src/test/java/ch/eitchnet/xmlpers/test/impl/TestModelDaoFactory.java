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

import java.text.MessageFormat;

import ch.eitchnet.xmlpers.api.AbstractDaoFactory;
import ch.eitchnet.xmlpers.api.XmlIoMode;
import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TestModelDaoFactory extends AbstractDaoFactory {

	@Override
	public <T> XmlPersistenceDao<T> getDao(T object) {

		XmlPersistenceDao<T> dao;
		if (object instanceof Resource) {
			dao = getDaoBy(object.getClass().getSimpleName(), ((Resource) object).getType());
		} else if (object instanceof Book) {
			dao = getDaoBy(Book.class.getSimpleName());
		} else {
			String msg = "The object with class {0} is not handled!";
			msg = MessageFormat.format(msg, object.getClass());
			throw new IllegalArgumentException(msg);
		}

		return dao;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> XmlPersistenceDao<T> getDaoBy(String type) {

		XmlPersistenceDao<T> dao;
		if (TestConstants.TYPE_BOOK.equals(type)) {
			XmlIoMode ioMode = getXmlIoMode();
			if (ioMode == XmlIoMode.DOM) {
				dao = (XmlPersistenceDao<T>) new BookDomDao();
			} else if (ioMode == XmlIoMode.SAX) {
				dao = (XmlPersistenceDao<T>) new BookSaxDao();
			} else {
				throw new IllegalArgumentException("The XmlIoMode " + ioMode + " is not yet supported!");
			}
		} else {
			String msg = "The object with type {0} is not handled!";
			msg = MessageFormat.format(msg, type);
			throw new IllegalArgumentException(msg);
		}

		return initializeDao(dao);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> XmlPersistenceDao<T> getDaoBy(String type, String subType) {

		XmlPersistenceDao<T> dao;
		if (TestConstants.TYPE_RES.equals(type)) {
			XmlIoMode ioMode = getXmlIoMode();
			if (ioMode == XmlIoMode.DOM) {
				dao = (XmlPersistenceDao<T>) new ResourceDomDao(subType);
			} else if (ioMode == XmlIoMode.SAX) {
				dao = (XmlPersistenceDao<T>) new ResourceSaxDao(subType);
			} else {
				throw new IllegalArgumentException("The XmlIoMode " + ioMode + " is not yet supported!");
			}
		} else {
			String msg = "The object with type {0} and subType {1} is not handled!";
			msg = MessageFormat.format(msg, type, subType);
			throw new IllegalArgumentException(msg);
		}

		return initializeDao(dao);
	}
}
