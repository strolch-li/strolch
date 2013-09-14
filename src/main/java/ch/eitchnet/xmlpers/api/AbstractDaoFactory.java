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

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.PropertiesHelper;
import ch.eitchnet.xmlpers.impl.MetadataXmlDao;
import ch.eitchnet.xmlpers.impl.XmlPersistenceDomHandler;
import ch.eitchnet.xmlpers.impl.XmlPersistenceFileDao;
import ch.eitchnet.xmlpers.impl.XmlPersistenceSaxHandler;
import ch.eitchnet.xmlpers.test.impl.TestModelDaoFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractDaoFactory implements XmlPersistenceDaoFactory {

	private static final Logger logger = LoggerFactory.getLogger(AbstractDaoFactory.class);

	private XmlIoMode xmlIoMode;
	private XmlPersistenceFileDao fileDao;

	@Override
	public void initialize(XmlPersistenceFileDao fileDao, Properties properties) {
		this.fileDao = fileDao;
		// TODO catch and throw proper exception
		String xmlIoModeS = PropertiesHelper.getProperty(properties, TestModelDaoFactory.class.getName(),
				XmlPersistenceConstants.PROP_XML_IO_MOD, XmlIoMode.SAX.name());
		this.xmlIoMode = XmlIoMode.valueOf(xmlIoModeS.toUpperCase());
		logger.info("Defaut Xml IO Mode is " + this.xmlIoMode.name());
	}

	/**
	 * @return
	 * @see ch.eitchnet.xmlpers.api.XmlPersistenceDaoFactory#getMetadataDao()
	 */
	@Override
	public XmlPersistenceMetadataDao getMetadataDao() {
		MetadataXmlDao metadataDao = new MetadataXmlDao();
		metadataDao.initialize(this.fileDao);
		return metadataDao;
	}

	protected XmlIoMode getXmlIoMode() {
		return this.xmlIoMode;
	}

	protected <T> XmlPersistenceDao<T> initializeDao(XmlPersistenceDao<T> dao) {
		if (!(dao instanceof AbstractXmlDao)) {
			throw new IllegalArgumentException("Your dao implementation does not extend from "
					+ AbstractXmlDao.class.getName() + "!");
		}
		AbstractXmlDao<T> abstractXmlDao = (AbstractXmlDao<T>) dao;
		abstractXmlDao.initialize(this.fileDao, getXmlFileHandler(this.xmlIoMode));
		return dao;
	}

	protected XmlPersistenceFileHandler getXmlFileHandler(XmlIoMode ioMode) {
		switch (ioMode) {
		case DOM:
			return new XmlPersistenceDomHandler();
		case SAX:
			return new XmlPersistenceSaxHandler();
		default:
			throw new IllegalArgumentException("The XmlIoMode " + ioMode + " is not yet supported!");
		}
	}
}