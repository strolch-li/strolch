/*
 * Copyright (c) 2010
 * 
 * Apixxo AG 
 * Hauptgasse 25
 * 4600 Olten
 * 
 * All rights reserved.
 * 
 */

/**
 * 
 */
package ch.eitchnet.featherlite.plugin.xmlpers.test;

import java.io.File;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.featherlite.plugin.xmlpers.test.impl.MyClass;
import ch.eitchnet.featherlite.plugin.xmlpers.test.impl.MyDaoFactory;
import ch.eitchnet.utils.helper.Log4jConfigurator;
import ch.eitchnet.utils.objectfilter.ITransactionObject;
import ch.eitchnet.xmlpers.XmlPersistenceExecption;
import ch.eitchnet.xmlpers.XmlPersistenceHandler;
import ch.eitchnet.xmlpers.XmlPersistenceTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class XmlPersistenceTest {

	private static final Logger logger = Logger.getLogger(XmlPersistenceTest.class.getName());

	private static XmlPersistenceHandler persistenceHandler;

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	@BeforeClass
	public static void init() throws Exception {

		try {
			// set up log4j
			Log4jConfigurator.configure();

			String userDir = System.getProperty("user.dir");
			String basePath = userDir + "/tmp/testdb";
			File basePathF = new File(basePath);
			if (!basePathF.exists() && !basePathF.mkdirs())
				Assert.fail("Could not create temporaray database store in " + basePathF.getAbsolutePath());

			System.setProperty(XmlPersistenceHandler.CONFIG_BASEPATH, "tmp/testdb");
			System.setProperty(XmlPersistenceHandler.CONFIG_VERBOSE, "true");
			System.setProperty(XmlPersistenceHandler.CONFIG_DAO_FACTORY_CLASS, MyDaoFactory.class.getName());

			persistenceHandler = new XmlPersistenceHandler();
			persistenceHandler.initialize();

			logger.info("Initialized persistence handler.");

		} catch (Exception e) {
			logger.error(e, e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	/**
	 * 
	 */
	@Test
	public void testCreate() {

		try {
			logger.info("Trying to create...");

			// new instance
			MyClass myClass = new MyClass("@id", "@name", "@subtype");

			// persist instance
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			tx.add(myClass);
			persistenceHandler.commitTx();

			logger.info("Done creating.");

		} catch (Exception e) {
			logger.error(e, e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	/**
	 * 
	 */
	@Test
	public void testRead() {

		try {
			logger.info("Trying to read...");

			// query MyClass with id @id
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			logger.info("Found MyClass: " + myClass);
			persistenceHandler.commitTx();

			logger.info("Done reading.");

		} catch (Exception e) {
			logger.error(e, e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	/**
	 * 
	 */
	@Test
	public void testUpdate() {

		try {
			logger.info("Trying to update an object...");

			// query the instance
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			logger.info("Found MyClass: " + myClass);

			// modify the instance
			myClass.setName("@name_modified");

			// update the instance
			tx.update(myClass);
			persistenceHandler.commitTx();

			logger.info("Done updating.");

		} catch (Exception e) {
			logger.error(e, e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

	/**
	 * 
	 */
	@Test
	public void testRemove() {

		logger.info("Trying to remove...");

		// query the instance
		XmlPersistenceTransaction tx = persistenceHandler.openTx();
		MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
		logger.info("Found MyClass: " + myClass);

		tx.remove(myClass);
		persistenceHandler.commitTx();

		logger.info("Done removing.");
	}

	/**
	 * 
	 */
	@Test(expected = XmlPersistenceExecption.class)
	public void testQueryFail() {

		try {
			logger.info("Trying to query removed object...");
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			MyClass myClass = tx.queryById(MyClass.class.getName(), "@subtype", "@id");
			logger.info("Found MyClass: " + myClass);
			logger.info("Done querying removed object");
		} finally {
			persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testReCreate() {

		try {
			logger.info("Trying to recreate...");

			// new instance
			MyClass myClass = new MyClass("@id", "@name", "@subtype");

			// persist instance
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			tx.add(myClass);
			persistenceHandler.commitTx();

			logger.info("Done creating.");

		} catch (Exception e) {
			logger.error(e, e);
			Assert.fail("Failed: " + e.getLocalizedMessage());
		}
	}

//	/**
//	 * 
//	 */
//	@Test
//	public void testQueryFromTo() {
//		Assert.fail("Not yet implemented");
//	}

	/**
	 * 
	 */
	@Test
	public void testQueryAll() {

		try {

			logger.info("Trying to query all...");

			// query all
			XmlPersistenceTransaction tx = persistenceHandler.openTx();
			List<Object> list = tx.queryAll(MyClass.class.getName());
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// also with subtype
			list = tx.queryAll(MyClass.class.getName(), "@subtype");
			Assert.assertTrue("Expected only one object, found " + list.size(), list.size() == 1);

			// and now something useless
			list = tx.queryAll(MyClass.class.getName(), "@inexistant");
			Assert.assertTrue("Expected no objects, found " + list.size(), list.size() == 0);

			logger.info("Done querying.");

		} finally {
			persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testKeySet() {

		try {
			XmlPersistenceTransaction tx = persistenceHandler.openTx();

			Set<String> keySet = tx.queryKeySet(MyClass.class.getName());
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// also with subtype
			keySet = tx.queryKeySet(MyClass.class.getName(), "@subtype");
			Assert.assertTrue("Expected one key, found " + keySet.size(), keySet.size() == 1);

			// and now something useless
			keySet = tx.queryKeySet(MyClass.class.getName(), "@inexistant");
			Assert.assertTrue("Expected no keys, found " + keySet, keySet.size() == 0);

		} finally {
			persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testRemoveAll() {

		try {
			XmlPersistenceTransaction tx = persistenceHandler.openTx();

			List<ITransactionObject> objects = tx.queryAll(MyClass.class.getName(), "@subType");
			tx.removeAll(objects);

		} finally {
			persistenceHandler.commitTx();
		}
	}

	/**
	 * 
	 */
	@Test
	public void testSize() {

		try {
			XmlPersistenceTransaction tx = persistenceHandler.openTx();

			long size = tx.querySize(MyClass.class.getName(), "@subType");
			Assert.assertTrue("Expected size = 0, found: " + size, size == 0);

		} finally {
			persistenceHandler.commitTx();
		}
	}
}
