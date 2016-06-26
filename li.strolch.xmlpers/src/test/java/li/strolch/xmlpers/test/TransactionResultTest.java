/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.xmlpers.test;

import static li.strolch.xmlpers.test.model.ModelBuilder.RES_ID;
import static li.strolch.xmlpers.test.model.ModelBuilder.createResource;
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

import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.ModificationResult;
import li.strolch.xmlpers.api.ObjectDao;
import li.strolch.xmlpers.api.PersistenceConstants;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.api.TransactionResult;
import li.strolch.xmlpers.test.model.Book;
import li.strolch.xmlpers.test.model.MyModel;

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
		assertThat(logMessage, containsString("TX for realm defaultRealm was completed after")); //$NON-NLS-1$
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
		List<MyModel> resources = new ArrayList<>(10);
		int i = 0;
		for (; i < 10; i++) {
			String id = RES_ID + "_" + i; //$NON-NLS-1$
			String name = "Tx Result Test 1 Object. " + i; //$NON-NLS-1$
			String type = "testTxResult1"; //$NON-NLS-1$

			MyModel resource = createResource(id, name, type);
			resources.add(resource);
		}
		for (; i < 20; i++) {
			String id = RES_ID + "_" + i; //$NON-NLS-1$
			String name = "Tx Result Test 2 Object. " + i; //$NON-NLS-1$
			String type = "testTxResult2"; //$NON-NLS-1$

			MyModel resource = createResource(id, name, type);
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
