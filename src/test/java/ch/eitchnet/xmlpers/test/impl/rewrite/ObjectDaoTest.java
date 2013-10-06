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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;

import java.util.Properties;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObjectDaoTest {

	private static final Logger logger = LoggerFactory.getLogger(ObjectDaoTest.class);

	private PersistenceContextFactory persistenceContextFactory;
	private XmlPersistenceManager persistenceManager;
	private PersistenceTransaction tx;

	@BeforeClass
	public static void beforeClass() {
	}

	@Before
	public void setUp() {

		this.persistenceContextFactory = new TestPersistenceContextFactory();

		Properties properties = new Properties();
		this.persistenceManager = XmlPersistenceManagerLoader.load(properties);
	}

	@After
	public void tearDown() {
		if (this.tx != null) {
			this.tx.rollback();
		}
	}

	@Test
	@Ignore
	public void testObjectDao() {

		this.tx = this.persistenceManager.openTx();

		Resource resource = createResource();

		this.tx.getObjectDao().add(resource);
		this.tx.commit(this.persistenceContextFactory);
	}
}
