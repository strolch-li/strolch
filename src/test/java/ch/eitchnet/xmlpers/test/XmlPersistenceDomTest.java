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
package ch.eitchnet.xmlpers.test;

import java.util.Properties;

import org.junit.BeforeClass;
import org.junit.Ignore;

import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.test.impl.TestModelDaoFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceDomTest extends AbstractXmlPersistenceTest {

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {

		Properties props = new Properties();
		props.setProperty(XmlPersistenceConstants.PROP_BASEPATH, "target/testdb");
		props.setProperty(XmlPersistenceConstants.PROP_VERBOSE, "true");
		props.setProperty(XmlPersistenceConstants.PROP_XML_IO_MOD, "dom");
		props.setProperty(XmlPersistenceConstants.PROP_DAO_FACTORY_CLASS, TestModelDaoFactory.class.getName());

		init(props);
	}
}
