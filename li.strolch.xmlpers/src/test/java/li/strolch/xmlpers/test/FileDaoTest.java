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

import static li.strolch.xmlpers.test.model.ModelBuilder.*;
import static org.junit.Assert.assertNull;

import java.util.Properties;

import li.strolch.xmlpers.api.*;
import li.strolch.xmlpers.objref.ObjectReferenceCache;
import li.strolch.xmlpers.test.model.MyModel;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("nls")
public class FileDaoTest extends AbstractPersistenceTest {

	private static final String TEST_PATH = "target/db/FileDaoTest/";
	private static final boolean VERBOSE = true;
	private static final boolean ALLOW_OVERWRITE_ON_CREATE = false;

	@BeforeClass
	public static void beforeClass() {
		cleanPath(TEST_PATH);
	}

	private void setup(IoMode ioMode) {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_XML_IO_MOD, ioMode.name());
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, TEST_PATH + ioMode.name());
		setup(properties);
	}

	@Test
	public void testCrudSax() {
		setup(IoMode.SAX);
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			FileDao fileDao = new FileDao(tx, this.persistenceManager.getPathBuilder(), VERBOSE,
					ALLOW_OVERWRITE_ON_CREATE);
			testCrud(tx, fileDao);
		}
	}

	@Test
	public void testCrudDom() {
		setup(IoMode.DOM);
		try (PersistenceTransaction tx = this.persistenceManager.openTx()) {
			FileDao fileDao = new FileDao(tx, this.persistenceManager.getPathBuilder(), VERBOSE,
					ALLOW_OVERWRITE_ON_CREATE);
			testCrud(tx, fileDao);
		}
	}

	private void testCrud(PersistenceTransaction tx, FileDao fileDao) {

		MyModel resource = createResource();
		assertResource(resource);
		Class<? extends MyModel> classType = resource.getClass();
		PersistenceContextFactory<MyModel> ctxFactory = tx.getManager().getCtxFactory().getCtxFactory(classType);
		ObjectReferenceCache objectRefCache = tx.getManager().getObjectRefCache();
		PersistenceContext<MyModel> context = ctxFactory.createCtx(objectRefCache, resource);
		context.setObject(resource);
		fileDao.performCreate(context);

		context.setObject(null);
		fileDao.performRead(context);
		assertResource(context.getObject());

		updateResource(context.getObject());
		fileDao.performUpdate(context);

		context.setObject(null);
		fileDao.performRead(context);
		assertResourceUpdated(context.getObject());

		fileDao.performDelete(context);

		context.setObject(null);
		fileDao.performRead(context);
		assertNull(context.getObject());

		context.setObject(createResource());
		fileDao.performCreate(context);
	}
}