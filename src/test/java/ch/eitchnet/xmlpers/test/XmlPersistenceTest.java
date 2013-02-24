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
import java.util.Set;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.objectfilter.ITransactionObject;
import ch.eitchnet.xmlpers.XmlPersistenceExecption;
import ch.eitchnet.xmlpers.XmlPersistenceHandler;
import ch.eitchnet.xmlpers.XmlPersistenceTransaction;
import ch.eitchnet.xmlpers.test.impl.MyClass;
import ch.eitchnet.xmlpers.test.impl.MyDaoFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceTest {

	private static final Logger logger = LoggerFactory.getLogger(XmlPersistenceTest.class.getName());

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

			System.setProperty(XmlPersistenceHandler.CONFIG_BASEPATH, "target/testdb");
			System.setProperty(XmlPersistenceHandler.CONFIG_VERBOSE, "true");
			System.setProperty(XmlPersistenceHandler.CONFIG_DAO_FACTORY_CLASS, MyDaoFactory.class.getName());

			XmlPersistenceTest.persistenceHandler = new XmlPersistenceHandler();
			XmlPersistenceTest.persistenceHandler.initialize();

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
			MyClass myClass = new MyClass("@id", "@name", "@subtype");

			// persist instance
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			tx.add(myClass);
			XmlPersistenceTest.persistenceHandler.commitTx();

			XmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	private void readObject() {

		try {
			XmlPersistenceTest.logger.info("Trying to read...");

			// query MyClass with id @id
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			XmlPersistenceTest.logger.info("Found MyClass: " + myClass);
			XmlPersistenceTest.persistenceHandler.commitTx();

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
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			XmlPersistenceTest.logger.info("Found MyClass: " + myClass);

			// modify the instance
			myClass.setName("@name_modified");

			// update the instance
			tx.update(myClass);
			XmlPersistenceTest.persistenceHandler.commitTx();

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
		MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
		XmlPersistenceTest.logger.info("Found MyClass: " + myClass);

		tx.remove(myClass);
		XmlPersistenceTest.persistenceHandler.commitTx();

		XmlPersistenceTest.logger.info("Done removing.");
	}

	/**
	 * 
	 */
	@Test
	public void testQueryFail() {

		try {
			XmlPersistenceTest.logger.info("Trying to query removed object...");
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			XmlPersistenceTest.logger.info("Found MyClass: " + myClass);
			XmlPersistenceTest.logger.info("Done querying removed object");
		} catch (XmlPersistenceExecption e) {
			Assert.assertEquals("Wrong error message. Expected error that object does not exist",
					"No object exists for ch.eitchnet.xmlpers.test.impl.MyClass / @subtype / @id",
					e.getLocalizedMessage());
		} finally {
			XmlPersistenceTest.persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testReCreate() {

		try {
			XmlPersistenceTest.logger.info("Trying to recreate...");

			// new instance
			MyClass myClass = new MyClass("@id", "@name", "@subtype");

			// persist instance
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			tx.add(myClass);
			XmlPersistenceTest.persistenceHandler.commitTx();

			XmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			XmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	/**
	 * 
	 */
	@Test
	@Ignore
	public void testQueryFromTo() {
		Assert.fail("Not yet implemented");
	}

	/**
	 * 
	 */
	@Test
	public void testQueryAll() {

		try {

			XmlPersistenceTest.logger.info("Trying to query all...");

			// query all
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();
			List<Object> list = tx.queryAll(MyClass.class.getName());
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// also with subtype
			list = tx.queryAll(MyClass.class.getName(), "@subtype");
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// and now something useless
			list = tx.queryAll(MyClass.class.getName(), "@inexistant");
			Assert.assertTrue("Expected no objects, found " + list.size(), list.size() == 0);

			XmlPersistenceTest.logger.info("Done querying.");

		} finally {
			XmlPersistenceTest.persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testKeySet() {

		try {
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();

			Set<String> keySet = tx.queryKeySet(MyClass.class.getName());
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// also with subtype
			keySet = tx.queryKeySet(MyClass.class.getName(), "@subtype");
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// and now something useless
			keySet = tx.queryKeySet(MyClass.class.getName(), "@inexistant");
			Assert.assertTrue("Expected no keys, found " + keySet, keySet.size() == 0);

		} finally {
			XmlPersistenceTest.persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testRemoveAll() {

		try {
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();

			List<ITransactionObject> objects = tx.queryAll(MyClass.class.getName(), "@subType");
			tx.removeAll(objects);

		} finally {
			XmlPersistenceTest.persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testSize() {

		try {
			XmlPersistenceTransaction tx = XmlPersistenceTest.persistenceHandler.openTx();

			long size = tx.querySize(MyClass.class.getName(), "@subType");
			Assert.assertTrue("Expected size = 0, found: " + size, size == 0);

		} finally {
			XmlPersistenceTest.persistenceHandler.commitTx();
		}
	}
}
