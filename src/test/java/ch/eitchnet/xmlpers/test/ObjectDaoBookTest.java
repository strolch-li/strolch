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
package ch.eitchnet.xmlpers.test;

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.BOOK_ID;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertBook;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertBookUpdated;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createBook;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.updateBook;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;

import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.ObjectDao;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.objref.IdOfTypeRef;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.objref.TypeRef;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.Book;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObjectDaoBookTest extends AbstractPersistenceTest {

	private static final String BASEPATH = "target/db/ObjectDaoTest/"; //$NON-NLS-1$

	@Before
	public void before() {
		cleanPath(BASEPATH);
	}

	private void setup(IoMode ioMode) {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASEPATH + ioMode.name());
		setup(properties);
	}

	@Test
	public void testCrudSax() {
		setup(IoMode.SAX);
		testCrud(IoMode.SAX);
	}

	@Test
	public void testCrudDom() {
		setup(IoMode.DOM);
		testCrud(IoMode.DOM);
	}

	private PersistenceTransaction freshTx(IoMode ioMode) {
		PersistenceTransaction tx = this.persistenceManager.openTx();
		tx.setIoMode(ioMode);
		return tx;
	}

	private void testCrud(IoMode ioMode) {

		ObjectDao objectDao;

		// create new book
		Book book = createBook();
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			objectDao = tx.getObjectDao();
			objectDao.add(book);
		}

		// read book
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			IdOfTypeRef bookRef = tx.getObjectRefCache()
					.getIdOfTypeRef(TestConstants.TYPE_BOOK, Long.toString(BOOK_ID));
			objectDao = tx.getObjectDao();
			book = objectDao.queryById(bookRef);
			assertBook(book);

			// modify book
			updateBook(book);
			objectDao.update(book);
		}

		// read modified book
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			IdOfTypeRef bookRef = tx.getObjectRefCache()
					.getIdOfTypeRef(TestConstants.TYPE_BOOK, Long.toString(BOOK_ID));
			objectDao = tx.getObjectDao();
			book = objectDao.queryById(bookRef);
			assertBookUpdated(book);
		}

		// delete book
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			objectDao = tx.getObjectDao();
			objectDao.remove(book);
		}

		// fail to read
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			IdOfTypeRef bookRef = tx.getObjectRefCache()
					.getIdOfTypeRef(TestConstants.TYPE_BOOK, Long.toString(BOOK_ID));
			objectDao = tx.getObjectDao();
			book = objectDao.queryById(bookRef);
			assertNull(book);

			// and create again
			book = createBook();
			assertBook(book);
			objectDao.add(book);
		}
	}

	@Test
	public void testBulkSax() {
		setup(IoMode.SAX);
		testBulk(IoMode.SAX);
	}

	@Test
	public void testBulkDom() {
		setup(IoMode.DOM);
		testBulk(IoMode.DOM);
	}

	private void testBulk(IoMode ioMode) {

		// create a list of books
		List<Book> books = new ArrayList<>(10);
		for (int i = 0; i < 10; i++) {
			long id = i;
			String title = "Bulk Test Book. " + i; //$NON-NLS-1$
			String author = "Nick Hornby"; //$NON-NLS-1$
			String press = "Penguin Books"; //$NON-NLS-1$
			double price = 21.30;

			Book book = createBook(id, title, author, press, price);
			books.add(book);
		}

		// save all
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			ObjectDao objectDao = tx.getObjectDao();
			objectDao.addAll(books);
			books.clear();
		}

		// query all
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			TypeRef typeRef = tx.getObjectRefCache().getTypeRef(TestConstants.TYPE_BOOK);
			ObjectDao objectDao = tx.getObjectDao();
			books = objectDao.queryAll(typeRef);
			assertEquals("Expected to find 10 entries!", 10, books.size()); //$NON-NLS-1$

			// delete them all
			objectDao.removeAll(books);
		}

		// now query them again
		try (PersistenceTransaction tx = freshTx(ioMode);) {
			TypeRef typeRef = tx.getObjectRefCache().getTypeRef(TestConstants.TYPE_BOOK);
			ObjectDao objectDao = tx.getObjectDao();
			books = objectDao.queryAll(typeRef);
			assertEquals("Expected to find 0 entries!", 0, books.size()); //$NON-NLS-1$
		}
	}

	@Test
	public void shouldPersistById() {
		setup(IoMode.SAX);

		String classType = TestConstants.TYPE_BOOK;
		long id = System.currentTimeMillis();
		String title = "About a boy"; //$NON-NLS-1$
		String author = "Nick Hornby"; //$NON-NLS-1$
		String press = "Penguin Books"; //$NON-NLS-1$
		double price = 21.30;

		// create a book
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			Book book = createBook(id, title, author, press, price);
			tx.getObjectDao().add(book);
		}

		// read by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getObjectRefCache().getIdOfTypeRef(classType, Long.toString(id));
			Book book = tx.getObjectDao().queryById(objectRef);
			assertNotNull("Expected to read book by ID", book); //$NON-NLS-1$
		}

		// delete by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getObjectRefCache().getIdOfTypeRef(classType, Long.toString(id));
			tx.getObjectDao().removeById(objectRef);
		}

		// fail to read by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getObjectRefCache().getIdOfTypeRef(classType, Long.toString(id));
			Book book = tx.getObjectDao().queryById(objectRef);
			assertNull("Expected that book was deleted by ID, thus can not be read anymore", book); //$NON-NLS-1$
		}
	}
}
