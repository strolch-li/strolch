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

import static li.strolch.xmlpers.test.impl.TestConstants.TYPE_RES;
import static li.strolch.xmlpers.test.model.ModelBuilder.*;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import li.strolch.xmlpers.api.*;
import li.strolch.xmlpers.objref.IdOfSubTypeRef;
import li.strolch.xmlpers.objref.ObjectRef;
import li.strolch.xmlpers.objref.SubTypeRef;
import li.strolch.xmlpers.test.impl.TestConstants;
import li.strolch.xmlpers.test.model.ModelBuilder;
import li.strolch.xmlpers.test.model.MyModel;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ObjectDaoResourceTest extends AbstractPersistenceTest {

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	private static final String BASEPATH = "target/db/ObjectDaoTest/"; //$NON-NLS-1$

	@BeforeClass
	public static void beforeClass() {
		cleanPath(BASEPATH);
	}

	private void setup(IoMode ioMode) {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, ioMode.name());
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASEPATH + ioMode.name());
		setup(properties);
	}

	@Test
	public void testCrudSax() {
		setup(IoMode.SAX);
		testCrud();
	}

	@Test
	public void testCrudDom() {
		setup(IoMode.DOM);
		testCrud();
	}

	private void testCrud() {

		ObjectDao objectDao;

		// create new resource
		MyModel resource = createResource();
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			objectDao = tx.getObjectDao();
			objectDao.add(resource);
		}

		// read resource
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			IdOfSubTypeRef resRef = tx.getManager().getObjectRefCache()
					.getIdOfSubTypeRef(TestConstants.TYPE_RES, RES_TYPE, RES_ID);
			objectDao = tx.getObjectDao();
			resource = objectDao.queryById(resRef);
			assertResource(resource);

			// modify resource
			updateResource(resource);
			objectDao.update(resource);
		}

		// read modified resource
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			IdOfSubTypeRef resRef = tx.getManager().getObjectRefCache()
					.getIdOfSubTypeRef(TestConstants.TYPE_RES, RES_TYPE, RES_ID);
			objectDao = tx.getObjectDao();
			resource = objectDao.queryById(resRef);
			assertResourceUpdated(resource);
		}

		// delete resource
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			objectDao = tx.getObjectDao();
			objectDao.remove(resource);
		}

		// fail to read
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			IdOfSubTypeRef resRef = tx.getManager().getObjectRefCache()
					.getIdOfSubTypeRef(TestConstants.TYPE_RES, RES_TYPE, RES_ID);
			objectDao = tx.getObjectDao();
			resource = objectDao.queryById(resRef);
			assertNull(resource);

			// and create again
			resource = createResource();
			assertResource(resource);
			objectDao.add(resource);
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

		// context
		String type = "testBulk" + ioMode.name(); //$NON-NLS-1$

		// create a list of resources
		List<MyModel> resources = new ArrayList<>(10);
		for (int i = 0; i < 10; i++) {
			String id = RES_ID + "_" + i; //$NON-NLS-1$
			String name = "Bulk Test Object. " + i; //$NON-NLS-1$

			MyModel resource = createResource(id, name, type);
			resources.add(resource);
		}

		// save all
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectDao objectDao = tx.getObjectDao();
			objectDao.addAll(resources);
			resources.clear();
		}

		// query all
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			SubTypeRef subTypeRef = tx.getManager().getObjectRefCache().getSubTypeRef(TestConstants.TYPE_RES, type);
			ObjectDao objectDao = tx.getObjectDao();
			resources = objectDao.queryAll(subTypeRef, file -> true);
			assertEquals("Expected to find 10 entries!", 10, resources.size()); //$NON-NLS-1$

			// delete them all
			objectDao.removeAll(resources);
		}

		// now query them again
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			SubTypeRef subTypeRef = tx.getManager().getObjectRefCache().getSubTypeRef(TestConstants.TYPE_RES, type);
			ObjectDao objectDao = tx.getObjectDao();
			resources = objectDao.queryAll(subTypeRef, file -> true);
			assertEquals("Expected to find 0 entries!", 0, resources.size()); //$NON-NLS-1$
		}
	}

	@Test
	public void shouldPersistById() {
		setup(IoMode.SAX);

		String classType = TestConstants.TYPE_RES;
		String subType = ModelBuilder.RES_TYPE;
		String id = "shouldPersistById"; //$NON-NLS-1$
		String name = "shouldPersistById "; //$NON-NLS-1$

		// create a resource
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			MyModel resource = createResource(id, name, subType);
			tx.getObjectDao().add(resource);
		}

		// read by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(classType, subType, id);
			MyModel resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull("Expected to read resource by ID", resource); //$NON-NLS-1$
		}

		// delete by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(classType, subType, id);
			tx.getObjectDao().removeById(objectRef);
		}

		// fail to read by id
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(classType, subType, id);
			MyModel resource = tx.getObjectDao().queryById(objectRef);
			assertNull("Expected that resource was deleted by ID, thus can not be read anymore",
					resource); //$NON-NLS-1$
		}
	}

	@Test
	public void shouldFailModifyNotExisting() {
		setup(IoMode.SAX);

		this.thrown.expect(XmlPersistenceException.class);
		this.thrown.expectMessage(containsString("Persistence unit does not exist for")); //$NON-NLS-1$

		// update
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			MyModel resource = createResource();
			tx.getObjectDao().update(resource);
		}
	}

	@Test
	public void shouldFailDeleteNotExisting() {
		setup(IoMode.SAX);

		this.thrown.expect(XmlPersistenceException.class);
		this.thrown.expectMessage(containsString("Persistence unit does not exist for")); //$NON-NLS-1$

		// delete
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			MyModel resource = createResource();
			tx.getObjectDao().remove(resource);
		}
	}

	@Test
	public void shouldAllowAllOperationsInSameTx() {
		setup(IoMode.SAX);

		String subType = ModelBuilder.RES_TYPE;
		String name = "shouldPersistById "; //$NON-NLS-1$

		// create
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			String id = "shouldAllowAllOperationsInSameTx_create"; //$NON-NLS-1$
			MyModel resource = createResource(id, name, subType);

			tx.getObjectDao().add(resource);
		}

		// create / modify
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			String id = "shouldAllowAllOperationsInSameTx_create_modify"; //$NON-NLS-1$
			MyModel resource = createResource(id, name, subType);

			tx.getObjectDao().add(resource);
			tx.getObjectDao().update(resource);
		}

		// create / delete
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			String id = "shouldAllowAllOperationsInSameTx_create_delete"; //$NON-NLS-1$
			MyModel resource = createResource(id, name, subType);

			tx.getObjectDao().add(resource);
			tx.getObjectDao().remove(resource);
		}

		// create / modify / delete 
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			String id = "shouldAllowAllOperationsInSameTx_create_modify_delete"; //$NON-NLS-1$
			MyModel resource = createResource(id, name, subType);

			tx.getObjectDao().add(resource);
			tx.getObjectDao().update(resource);
			tx.getObjectDao().remove(resource);
		}

		String id = "shouldAllowAllOperationsInSameTx_read_modify"; //$NON-NLS-1$

		// prepare for read/modify
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			MyModel resource = createResource(id, name, subType);
			tx.getObjectDao().add(resource);
		}

		// read / modify
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(TYPE_RES, subType, id);
			Object resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull(resource);
			tx.getObjectDao().update(resource);
		}

		// read / delete
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(TYPE_RES, subType, id);
			Object resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull(resource);
			tx.getObjectDao().remove(resource);
		}

		// make sure deleted, then recreate
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(TYPE_RES, subType, id);
			Object resource = tx.getObjectDao().queryById(objectRef);
			assertNull(resource);

			// recreate
			resource = createResource(id, name, subType);
			tx.getObjectDao().add(resource);
		}

		// read / modify / delete
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {

			ObjectRef objectRef = tx.getManager().getObjectRefCache().getIdOfSubTypeRef(TYPE_RES, subType, id);
			Object resource = tx.getObjectDao().queryById(objectRef);
			assertNotNull(resource);
			tx.getObjectDao().update(resource);
			tx.getObjectDao().remove(resource);
		}
	}
}
