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

import static li.strolch.xmlpers.test.model.ModelBuilder.assertResource;
import static li.strolch.xmlpers.test.model.ModelBuilder.assertResourceUpdated;
import static li.strolch.xmlpers.test.model.ModelBuilder.createResource;
import static li.strolch.xmlpers.test.model.ModelBuilder.updateResource;
import static org.junit.Assert.assertNull;

import java.util.Properties;

import org.junit.BeforeClass;
import org.junit.Test;

import li.strolch.xmlpers.api.FileDao;
import li.strolch.xmlpers.api.IoMode;
import li.strolch.xmlpers.api.PersistenceConstants;
import li.strolch.xmlpers.api.PersistenceContext;
import li.strolch.xmlpers.api.PersistenceContextFactory;
import li.strolch.xmlpers.api.PersistenceContextFactoryDelegator;
import li.strolch.xmlpers.api.PersistenceManager;
import li.strolch.xmlpers.api.PersistenceTransaction;
import li.strolch.xmlpers.impl.DefaultPersistenceRealm;
import li.strolch.xmlpers.impl.DefaultPersistenceTransaction;
import li.strolch.xmlpers.impl.PathBuilder;
import li.strolch.xmlpers.objref.ObjectReferenceCache;
import li.strolch.xmlpers.test.model.MyModel;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class FileDaoTest extends AbstractPersistenceTest {

	private static final String TEST_PATH = "target/db/FileDaoTest/";
	private static final boolean VERBOSE = true;
	private DefaultPersistenceRealm realm;
	private PathBuilder pathBuilder;

	@BeforeClass
	public static void beforeClass() {
		cleanPath(TEST_PATH);
	}

	private void setup(IoMode ioMode) {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, TEST_PATH + ioMode.name());
		setup(properties);

		ObjectReferenceCache objectRefCache = new ObjectReferenceCache(PersistenceManager.DEFAULT_REALM);
		this.pathBuilder = new PathBuilder(PersistenceManager.DEFAULT_REALM, properties);
		this.realm = new DefaultPersistenceRealm(PersistenceManager.DEFAULT_REALM, this.persistenceManager,
				this.persistenceManager.getCtxFactory(), this.pathBuilder, objectRefCache);
	}

	@Test
	public void testCrudSax() {
		setup(IoMode.SAX);
		try (PersistenceTransaction tx = new DefaultPersistenceTransaction(this.realm, VERBOSE)) {
			FileDao fileDao = new FileDao(tx, this.pathBuilder, VERBOSE);
			tx.setIoMode(IoMode.SAX);
			testCrud(this.realm.getCtxFactoryDelegator(), fileDao);
		}
	}

	@Test
	public void testCrudDom() {
		setup(IoMode.DOM);
		try (PersistenceTransaction tx = new DefaultPersistenceTransaction(this.realm, VERBOSE)) {
			FileDao fileDao = new FileDao(tx, this.pathBuilder, VERBOSE);
			tx.setIoMode(IoMode.DOM);
			testCrud(this.realm.getCtxFactoryDelegator(), fileDao);
		}
	}

	private void testCrud(PersistenceContextFactoryDelegator ctxFactoryDelegator, FileDao fileDao) {

		MyModel resource = createResource();
		assertResource(resource);
		Class<? extends MyModel> classType = resource.getClass();
		PersistenceContextFactory<MyModel> ctxFactory = ctxFactoryDelegator.getCtxFactory(classType);
		ObjectReferenceCache objectRefCache = this.realm.getObjectRefCache();
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