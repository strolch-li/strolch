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

import java.io.File;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.api.XmlPersistenceException;
import ch.eitchnet.xmlpers.api.XmlPersistenceHandler;
import ch.eitchnet.xmlpers.api.XmlPersistenceTransaction;
import ch.eitchnet.xmlpers.test.impl.TestModelDaoFactory;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceTest {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceTest.class.getName());

	private static final String RES_TYPE = "@subType";
	private static final String RES_TYPE_INEXISTANT = "@inexistant";
	private static final String RES_NAME = "@name";
	private static final String RES_NAME_MODIFIED = "@name_modified";
	private static final String RES_ID = "@id";

	private static XmlPersistenceHandler persistenceHandler;

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {
		try {
			String userDir = System.getProperty("user.dir");
			String basePath = userDir + "/target/testdb";
			File basePathF = new File(basePath);
			if (!basePathF.exists() && !basePathF.mkdirs())
				Assert.fail("Could not create temporaray database store in " + basePathF.getAbsolutePath());

			Properties props = new Properties();
			props.setProperty(XmlPersistenceConstants.PROP_BASEPATH, "target/testdb");
			props.setProperty(XmlPersistenceConstants.PROP_VERBOSE, "true");
			props.setProperty(XmlPersistenceConstants.PROP_DAO_FACTORY_CLASS, TestModelDaoFactory.class.getName());

			XmlPersistenceTest.persistenceHandler = new XmlPersistenceHandler();
			XmlPersistenceTest.persistenceHandler.initialize(props);
			XmlPersistenceTest.logger.info("Initialized persistence handler.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Tests the following story:
	 * <ul>
	 * <li>create object</li>
	 * <li>read object</li>
	 * <li>update object</li>
	 * <li>remove object</li>
	 * </ul>
	 * 
	 */
	@Test
	public void testPersistenceStory() {

		createObject();
		readObject();
		updateObject();
		removeObject();
	}

	private void createObject() {

		try {
			XmlPersistenceTest.logger.info("Trying to create...");

			// new instance
			Resource resource = new Resource(RES_ID, RES_NAME, RES_TYPE);

			// persist instance
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			tx.add(resource);
			tx.commit();

			XmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	private void readObject() {

		try {
			XmlPersistenceTest.logger.info("Trying to read...");

			// query Resource with id @id
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			Resource resource = tx.queryById(Resource.class.getName(), RES_TYPE, RES_ID);
			XmlPersistenceTest.logger.info("Found Resource: " + resource);
			tx.commit();

			XmlPersistenceTest.logger.info("Done reading.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	private void updateObject() {

		try {
			XmlPersistenceTest.logger.info("Trying to update an object...");

			// query the instance
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			Resource resource = tx.queryById(Resource.class.getName(), RES_TYPE, RES_ID);
			XmlPersistenceTest.logger.info("Found Resource: " + resource);

			// modify the instance
			resource.setName(RES_NAME_MODIFIED);

			// update the instance
			tx.update(resource);
			tx.commit();

			XmlPersistenceTest.logger.info("Done updating.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	private void removeObject() {

		XmlPersistenceTest.logger.info("Trying to remove...");

		// query the instance
		XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
		Resource resource = tx.queryById(Resource.class.getName(), RES_TYPE, RES_ID);
		XmlPersistenceTest.logger.info("Found Resource: " + resource);

		tx.remove(resource);
		tx.commit();

		XmlPersistenceTest.logger.info("Done removing.");
	}

	@Test
	public void testQueryFail() {

		XmlPersistenceTransaction tx = null;
		try {
			XmlPersistenceTest.logger.info("Trying to query removed object...");
			tx = XmlPersistenceTest.persistenceHandler.openTx();
			Resource resource = tx.queryById(Resource.class.getName(), RES_TYPE, RES_ID);
			XmlPersistenceTest.logger.info("Found Resource: " + resource);
			XmlPersistenceTest.logger.info("Done querying removed object");
		} catch (XmlPersistenceException e) {
			Assert.assertEquals("Wrong error message. Expected error that object does not exist",
					"No object exists for ch.eitchnet.xmlpers.test.impl.Resource / @subtype / @id",
					e.getLocalizedMessage());
		} finally {
			if (tx != null)
				tx.commit();
		}
	}

	@Test
	public void testReCreate() {

		try {
			XmlPersistenceTest.logger.info("Trying to recreate...");

			// new instance
			Resource resource = new Resource(RES_ID, RES_NAME, RES_TYPE);

			// persist instance
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			tx.add(resource);
			tx.commit();

			XmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	@Test
	@Ignore
	public void testQueryFromTo() {
		Assert.fail("Not yet implemented");
	}

	@Test
	public void testQueryAll() {

		XmlPersistenceTransaction tx = null;
		try {

			XmlPersistenceTest.logger.info("Trying to query all...");

			// query all
			tx = XmlPersistenceTest.persistenceHandler.openTx();
			List<Object> list = tx.queryAll(Resource.class.getName());
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// also with subtype
			list = tx.queryAll(Resource.class.getName(), RES_TYPE);
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// and now something useless
			list = tx.queryAll(Resource.class.getName(), RES_TYPE_INEXISTANT);
			Assert.assertTrue("Expected no objects, found " + list.size(), list.size() == 0);

			XmlPersistenceTest.logger.info("Done querying.");

		} finally {
			if (tx != null)
				tx.commit();
		}
	}

	@Test
	public void testKeySet() {

		XmlPersistenceTransaction tx = null;
		try {
			tx = XmlPersistenceTest.persistenceHandler.openTx();

			Set<String> keySet = tx.queryKeySet(Resource.class.getName());
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// also with subtype
			keySet = tx.queryKeySet(Resource.class.getName(), RES_TYPE);
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// and now something useless
			keySet = tx.queryKeySet(Resource.class.getName(), RES_TYPE_INEXISTANT);
			Assert.assertTrue("Expected no keys, found " + keySet, keySet.size() == 0);

		} finally {
			if (tx != null)
				tx.commit();
		}
	}

	@Test
	public void testRemoveAll() {

		XmlPersistenceTransaction tx = null;
		try {
			tx = XmlPersistenceTest.persistenceHandler.openTx();

			List<Resource> objects = tx.queryAll(Resource.class.getName(), RES_TYPE);
			tx.removeAll(objects);

		} finally {
			if (tx != null)
				tx.commit();
		}
	}

	@Test
	public void testSize() {

		XmlPersistenceTransaction tx = null;
		try {
			tx = XmlPersistenceTest.persistenceHandler.openTx();

			long size = tx.querySize(Resource.class.getName(), RES_TYPE);
			Assert.assertTrue("Expected size = 0, found: " + size, size == 0);

		} finally {
			if (tx != null)
				tx.commit();
		}
	}
}
