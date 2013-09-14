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

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.xmlpers.api.XmlPersistenceDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceHandler;
import ch.eitchnet.xmlpers.api.XmlPersistenceMetadataDao;
import ch.eitchnet.xmlpers.api.XmlPersistenceTransaction;
import ch.eitchnet.xmlpers.impl.XmlPersistenceHandlerImpl;
import ch.eitchnet.xmlpers.test.impl.Book;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.Parameter;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AbstractXmlPersistenceTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractXmlPersistenceTest.class.getName());

	protected static final String RES_TYPE = "@subType";
	protected static final String RES_TYPE_INEXISTANT = "@inexistant";
	protected static final String RES_NAME = "@name";
	protected static final String RES_NAME_MODIFIED = "@name_modified";
	protected static final String RES_ID = "@id";

	protected static final String PARAM_TYPE = "@paramType";
	protected static final String PARAM_NAME = "@paramName";
	protected static final String PARAM_ID = "@paramId";
	protected static final String PARAM_VALUE_1 = "@paramValue1";
	protected static final String PARAM_VALUE_2 = "@paramValue2";

	protected static final long BOOK_ID = 10L;
	protected static final String BOOK_TITLE = "Nick Hornby";
	protected static final String BOOK_AUTHOR = "A long way down";
	protected static final String BOOK_PRESS_1 = "Some press";
	protected static final String BOOK_PRESS_2 = "Another press";
	protected static final double BOOK_PRICE = 45.55D;

	protected static XmlPersistenceHandler persistenceHandler;

	protected XmlPersistenceTransaction tx;

	/**
	 * @throws Exception
	 *             if something goes wrong
	 */
	public static void init(Properties props) throws Exception {
		try {
			String userDir = System.getProperty("user.dir");
			String basePath = userDir + "/target/testdb";
			File basePathF = new File(basePath);
			if (!basePathF.exists() && !basePathF.mkdirs())
				Assert.fail("Could not create temporaray database store in " + basePathF.getAbsolutePath());

			AbstractXmlPersistenceTest.persistenceHandler = new XmlPersistenceHandlerImpl();
			((XmlPersistenceHandlerImpl) AbstractXmlPersistenceTest.persistenceHandler).initialize(props);
			AbstractXmlPersistenceTest.logger.info("Initialized persistence handler.");

		} catch (Exception e) {
			AbstractXmlPersistenceTest.logger.error(e.getMessage(), e);

			throw new RuntimeException("Initialization failed: " + e.getLocalizedMessage(), e);
		}
	}

	@AfterClass
	public static void afterClass() {
		String userDir = System.getProperty("user.dir");
		String basePath = userDir + "/target/testdb";
		File basePathF = new File(basePath);
		if (basePathF.exists() && !FileHelper.deleteFile(basePathF, true))
			Assert.fail("Could not delete temporaray database store at " + basePathF.getAbsolutePath());
	}

	@After
	public void tearDown() {
		if (this.tx != null)
			this.tx.clear();
	}

	/**
	 * Tests the following story:
	 * <ul>
	 * <li>create book</li>
	 * <li>read book</li>
	 * <li>update book</li>
	 * <li>remove book</li>
	 * </ul>
	 */
	@Test
	public void testBookPersistence() {

		// create
		Book book = createBook();
		assertBook(book);
		this.tx = persistenceHandler.openTx();
		this.tx.add(book);
		this.tx.commit();

		// read
		this.tx = persistenceHandler.openTx();
		XmlPersistenceDao<Book> dao = this.tx.getDaoFactory().getDao(book);
		Book persistedBook = dao.queryById(String.valueOf(BOOK_ID));
		assertBook(persistedBook);

		// update
		persistedBook.setPress(BOOK_PRESS_2);
		this.tx.update(persistedBook);
		this.tx.commit();

		// read
		this.tx = persistenceHandler.openTx();
		dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_BOOK);
		persistedBook = dao.queryById(String.valueOf(BOOK_ID));
		assertBookUpdated(persistedBook);

		// delete
		this.tx.remove(book);
		this.tx.commit();

		// fail to read
		this.tx = persistenceHandler.openTx();
		dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_BOOK);
		persistedBook = dao.queryById(String.valueOf(BOOK_ID));
		Assert.assertNull(persistedBook);
	}

	/**
	 * Tests the following story:
	 * <ul>
	 * <li>create resource</li>
	 * <li>read resource</li>
	 * <li>update resource</li>
	 * <li>remove resource</li>
	 * </ul>
	 */
	@Test
	public void testResourcePersistence() {

		persistResource();
		readResource();
		updateResource();
		removeResource();
	}

	private void persistResource() {

		try {
			AbstractXmlPersistenceTest.logger.info("Trying to create...");

			// new instance
			Resource resource = createResource();
			assertResource(resource);

			// persist instance
			this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
			this.tx.add(resource);
			this.tx.commit();

			AbstractXmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			AbstractXmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed by exception: " + e.getLocalizedMessage());
		}
	}

	private void readResource() {

		try {
			AbstractXmlPersistenceTest.logger.info("Trying to read...");

			// query Resource
			this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
			XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
			Resource resource = dao.queryById(RES_ID);
			AbstractXmlPersistenceTest.logger.info("Found Resource: " + resource);
			assertResource(resource);
			this.tx.commit();

			AbstractXmlPersistenceTest.logger.info("Done reading.");

		} catch (Exception e) {
			AbstractXmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed by exception: " + e.getLocalizedMessage());
		}
	}

	private void updateResource() {

		try {
			AbstractXmlPersistenceTest.logger.info("Trying to update an object...");

			// query the instance
			this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
			XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
			Resource resource = dao.queryById(RES_ID);
			AbstractXmlPersistenceTest.logger.info("Found Resource: " + resource);
			assertResource(resource);

			// modify the instance
			resource.setName(RES_NAME_MODIFIED);
			resource.getParameterBy(PARAM_ID).setValue(PARAM_VALUE_2);

			// update the instance
			this.tx.update(resource);
			this.tx.commit();

			// re-read and validate
			this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
			dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
			resource = dao.queryById(RES_ID);
			this.tx.commit();
			AbstractXmlPersistenceTest.logger.info("Found Resource: " + resource);
			assertResourceUpdated(resource);

			AbstractXmlPersistenceTest.logger.info("Done updating.");

		} catch (Exception e) {
			AbstractXmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed by exception: " + e.getLocalizedMessage());
		}
	}

	private void removeResource() {

		AbstractXmlPersistenceTest.logger.info("Trying to remove...");

		// query the instance
		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
		Resource resource = dao.queryById(RES_ID);
		AbstractXmlPersistenceTest.logger.info("Found Resource: " + resource);
		assertResourceUpdated(resource);

		this.tx.remove(resource);
		this.tx.commit();

		AbstractXmlPersistenceTest.logger.info("Done removing.");
	}

	@Test
	public void testQueryFail() {

		AbstractXmlPersistenceTest.logger.info("Trying to query removed object...");
		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
		Resource resource = dao.queryById(RES_ID);
		this.tx.commit();
		Assert.assertNull("Expected resource not to be found!", resource);
	}

	@Test
	public void testReCreate() {

		try {
			AbstractXmlPersistenceTest.logger.info("Trying to recreate...");

			Resource resource = createResource();
			assertResource(resource);

			// persist instance
			this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
			this.tx.add(resource);
			this.tx.commit();

			AbstractXmlPersistenceTest.logger.info("Done creating.");

		} catch (Exception e) {
			AbstractXmlPersistenceTest.logger.error(e.getMessage(), e);
			Assert.fail("Failed by exception: " + e.getLocalizedMessage());
		}
	}

	@Test
	@Ignore
	public void testQueryFromTo() {
		Assert.fail("Not yet implemented");
	}

	@Test
	public void testQueryAll() {

		AbstractXmlPersistenceTest.logger.info("Trying to query all...");

		// query all
		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
		List<Resource> list = dao.queryAll();
		Assert.assertEquals("Expected only one object, found " + list, 1, list.size());

		// and now something useless
		dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE_INEXISTANT);
		list = dao.queryAll();
		this.tx.commit();
		Assert.assertEquals("Expected no objects, found " + list, 0, list.size());

		AbstractXmlPersistenceTest.logger.info("Done querying.");
	}

	@Test
	public void testKeySet() {

		// first prepare by creating a resource
		createResource();

		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceMetadataDao metadataDao = this.tx.getDaoFactory().getMetadataDao();

		// query by type only, which should return nothing on this level
		Set<String> keySet = metadataDao.queryKeySet(TestConstants.TYPE_RES);
		Assert.assertEquals("A resource can only be queried by type/subtype, but dao returned values!", 0,
				keySet.size());

		// now we shoud find our resource with the given type
		keySet = metadataDao.queryKeySet(TestConstants.TYPE_RES, RES_TYPE);
		Assert.assertEquals("Expected one key, found " + keySet, 1, keySet.size());

		// and now something useless
		keySet = metadataDao.queryKeySet(TestConstants.TYPE_RES, RES_TYPE_INEXISTANT);
		Assert.assertEquals("Expected no keys, found " + keySet, 0, keySet.size());

		this.tx.commit();
	}

	@Test
	public void testRemoveAll() {

		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceDao<Resource> dao = this.tx.getDaoFactory().getDaoBy(TestConstants.TYPE_RES, RES_TYPE);
		List<Resource> objects = dao.queryAll();
		this.tx.removeAll(objects);
		this.tx.commit();
	}

	@Test
	public void testSize() {

		this.tx = AbstractXmlPersistenceTest.persistenceHandler.openTx();
		XmlPersistenceMetadataDao metadataDao = this.tx.getDaoFactory().getMetadataDao();
		long size = metadataDao.querySize(TestConstants.TYPE_RES, RES_TYPE);
		this.tx.commit();
		Assert.assertEquals("Expected size = 0, found: " + size, 0, size);
	}

	private Book createBook() {
		Book book = new Book(BOOK_ID, BOOK_TITLE, BOOK_AUTHOR, BOOK_PRESS_1, BOOK_PRICE);
		return book;
	}

	private void assertBook(Book book) {
		Assert.assertNotNull(book);
		Assert.assertEquals(BOOK_ID, book.getId().longValue());
		Assert.assertEquals(BOOK_TITLE, book.getTitle());
		Assert.assertEquals(BOOK_AUTHOR, book.getAuthor());
		Assert.assertEquals(BOOK_PRESS_1, book.getPress());
		Assert.assertEquals(BOOK_PRICE, book.getPrice(), 0.0);
	}

	private void assertBookUpdated(Book book) {
		Assert.assertNotNull(book);
		Assert.assertEquals(BOOK_ID, book.getId().longValue());
		Assert.assertEquals(BOOK_TITLE, book.getTitle());
		Assert.assertEquals(BOOK_AUTHOR, book.getAuthor());
		Assert.assertEquals(BOOK_PRESS_2, book.getPress());
		Assert.assertEquals(BOOK_PRICE, book.getPrice(), 0.0);
	}

	private Resource createResource() {
		Resource resource = new Resource(RES_ID, RES_NAME, RES_TYPE);
		Parameter param = new Parameter(PARAM_ID, PARAM_NAME, PARAM_TYPE, PARAM_VALUE_1);
		resource.addParameter(param);
		return resource;
	}

	private void assertResource(Resource resource) {
		Assert.assertNotNull(resource);
		Assert.assertEquals(RES_ID, resource.getId());
		Assert.assertEquals(RES_NAME, resource.getName());
		Assert.assertEquals(RES_TYPE, resource.getType());
		Parameter param = resource.getParameterBy(PARAM_ID);
		Assert.assertNotNull(param);
		Assert.assertEquals(PARAM_ID, param.getId());
		Assert.assertEquals(PARAM_NAME, param.getName());
		Assert.assertEquals(PARAM_TYPE, param.getType());
		Assert.assertEquals(PARAM_VALUE_1, param.getValue());
	}

	private void assertResourceUpdated(Resource resource) {
		Assert.assertNotNull(resource);
		Assert.assertEquals(RES_ID, resource.getId());
		Assert.assertEquals(RES_NAME_MODIFIED, resource.getName());
		Assert.assertEquals(RES_TYPE, resource.getType());
		Parameter param = resource.getParameterBy(PARAM_ID);
		Assert.assertNotNull(param);
		Assert.assertEquals(PARAM_ID, param.getId());
		Assert.assertEquals(PARAM_NAME, param.getName());
		Assert.assertEquals(PARAM_TYPE, param.getType());
		Assert.assertEquals(PARAM_VALUE_2, param.getValue());
	}
}
