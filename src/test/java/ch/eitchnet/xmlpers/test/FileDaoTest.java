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

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertResource;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertResourceUpdated;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.updateResource;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.util.Properties;

import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.xmlpers.api.FileDao;
import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.PersistenceContextFactory;
import ch.eitchnet.xmlpers.api.PersistenceContextFactoryDelegator;
import ch.eitchnet.xmlpers.api.PersistenceManager;
import ch.eitchnet.xmlpers.api.PersistenceManagerLoader;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.impl.DefaultPersistenceRealm;
import ch.eitchnet.xmlpers.impl.DefaultPersistenceTransaction;
import ch.eitchnet.xmlpers.impl.PathBuilder;
import ch.eitchnet.xmlpers.objref.ObjectReferenceCache;
import ch.eitchnet.xmlpers.test.impl.ResourceContextFactory;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.Resource;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
@SuppressWarnings("nls")
public class FileDaoTest {

	private static final String TEST_PATH = "target/dbTest";
	private static final boolean VERBOSE = true;
	private static Properties properties;
	private PersistenceManager persistenceManager;
	private DefaultPersistenceRealm realm;
	private PathBuilder pathBuilder;

	@BeforeClass
	public static void beforeClass() {
		File file = new File(TEST_PATH).getAbsoluteFile();
		if (file.exists() && file.isDirectory())
			if (!FileHelper.deleteFiles(file.listFiles(), true))
				throw new RuntimeException("Could not clean up path " + file.getAbsolutePath());

		if (!file.exists() && !file.mkdir())
			throw new RuntimeException("Failed to create path " + file);

		File domFile = new File(file, "dom");
		if (!domFile.mkdir())
			throw new RuntimeException("Failed to create path " + domFile);

		File saxFile = new File(file, "sax");
		if (!saxFile.mkdir())
			throw new RuntimeException("Failed to create path " + saxFile);

		properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_VERBOSE, "true"); //$NON-NLS-1$
	}

	private void setup(String subPath) {
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, TEST_PATH + subPath);
		this.persistenceManager = PersistenceManagerLoader.load(properties);
		this.persistenceManager.getCtxFactory().registerPersistenceContextFactory(Resource.class,
				TestConstants.TYPE_RES, new ResourceContextFactory());

		ObjectReferenceCache objectRefCache = new ObjectReferenceCache(PersistenceManager.DEFAULT_REALM);
		this.pathBuilder = new PathBuilder(PersistenceManager.DEFAULT_REALM, FileDaoTest.properties);
		this.realm = new DefaultPersistenceRealm(PersistenceManager.DEFAULT_REALM, this.persistenceManager,
				this.persistenceManager.getCtxFactory(), this.pathBuilder, objectRefCache);
	}

	@Test
	public void testCrudSax() {
		setup("/sax/");
		try (PersistenceTransaction tx = new DefaultPersistenceTransaction(this.realm, VERBOSE)) {
			FileDao fileDao = new FileDao(tx, this.pathBuilder, VERBOSE);
			tx.setIoMode(IoMode.SAX);
			testCrud(tx, this.realm.getCtxFactoryDelegator(), fileDao);
		}
	}

	@Test
	public void testCrudDom() {
		setup("/dom/");
		try (PersistenceTransaction tx = new DefaultPersistenceTransaction(this.realm, VERBOSE)) {
			FileDao fileDao = new FileDao(tx, this.pathBuilder, VERBOSE);
			tx.setIoMode(IoMode.DOM);
			testCrud(tx, this.realm.getCtxFactoryDelegator(), fileDao);
		}
	}

	private void testCrud(PersistenceTransaction tx, PersistenceContextFactoryDelegator ctxFactoryDelegator,
			FileDao fileDao) {

		Resource resource = createResource();
		assertResource(resource);
		Class<? extends Resource> classType = resource.getClass();
		PersistenceContextFactory<Resource> ctxFactory = ctxFactoryDelegator.getCtxFactory(classType);
		ObjectReferenceCache objectRefCache = this.realm.getObjectRefCache();
		PersistenceContext<Resource> context = ctxFactory.createCtx(objectRefCache, resource);
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