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

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.RES_ID;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.ModificationResult;
import ch.eitchnet.xmlpers.api.ObjectDao;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.api.TransactionResult;
import ch.eitchnet.xmlpers.test.model.Book;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class TransactionResultTest extends AbstractPersistenceTest {

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	private static final String BASEPATH = "target/db/TxResultTest/"; //$NON-NLS-1$

	@Before
	public void setup() {
		cleanPath(BASEPATH);
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASEPATH);
		setup(properties);
	}

	private PersistenceTransaction freshTx() {
		PersistenceTransaction tx = this.persistenceManager.openTx();
		tx.setIoMode(IoMode.SAX);
		return tx;
	}

	@Test
	public void testWithTxResult() {

		TransactionResult txResult = new TransactionResult();
		performChanges(txResult);
		String logMessage = txResult.getLogMessage();
		logger.info(logMessage);
		assertThat(logMessage, containsString("TX was completed after")); //$NON-NLS-1$
		assertThat(logMessage, containsString("30 objects in 2 types were modified")); //$NON-NLS-1$
		assertThat(txResult.getKeys(), containsInAnyOrder("Resource", "Book")); //$NON-NLS-1$ //$NON-NLS-2$

		ModificationResult resourceModificationResult = txResult.getModificationResult("Resource"); //$NON-NLS-1$
		assertEquals(20, resourceModificationResult.getCreated().size());
		assertEquals(0, resourceModificationResult.getUpdated().size());
		assertEquals(0, resourceModificationResult.getDeleted().size());

		ModificationResult bookModificationResult = txResult.getModificationResult("Book"); //$NON-NLS-1$
		assertEquals(10, bookModificationResult.getCreated().size());
		assertEquals(0, bookModificationResult.getUpdated().size());
		assertEquals(0, bookModificationResult.getDeleted().size());
	}

	@Test
	public void testWithoutTxResult() {
		performChanges(null);
	}

	private void performChanges(TransactionResult txResult) {

		// create a list of resources
		List<Resource> resources = new ArrayList<>(10);
		int i = 0;
		for (; i < 10; i++) {
			String id = RES_ID + "_" + i; //$NON-NLS-1$
			String name = "Tx Result Test 1 Object. " + i; //$NON-NLS-1$
			String type = "testTxResult1"; //$NON-NLS-1$

			Resource resource = createResource(id, name, type);
			resources.add(resource);
		}
		for (; i < 20; i++) {
			String id = RES_ID + "_" + i; //$NON-NLS-1$
			String name = "Tx Result Test 2 Object. " + i; //$NON-NLS-1$
			String type = "testTxResult2"; //$NON-NLS-1$

			Resource resource = createResource(id, name, type);
			resources.add(resource);
		}

		// create a list of books
		List<Book> books = new ArrayList<>(10);
		i = 0;
		for (; i < 10; i++) {
			String title = "Tx Result Test Book " + i; //$NON-NLS-1$
			Book book = new Book((long) i, title, "Douglas Adams", "Apress", Math.random() * i); //$NON-NLS-1$ //$NON-NLS-2$
			books.add(book);
		}

		// save all
		try (PersistenceTransaction tx = freshTx();) {
			tx.setTransactionResult(txResult);
			ObjectDao objectDao = tx.getObjectDao();
			objectDao.addAll(resources);
			objectDao.addAll(books);
			resources.clear();
		}
	}
}
