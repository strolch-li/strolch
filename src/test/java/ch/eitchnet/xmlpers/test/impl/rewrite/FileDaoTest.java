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
package ch.eitchnet.xmlpers.test.impl.rewrite;

import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertResource;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.assertResourceUpdated;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.createResource;
import static ch.eitchnet.xmlpers.test.model.ModelBuilder.updateResource;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.util.Properties;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.utils.helper.FileHelper;
import ch.eitchnet.xmlpers.api.PersistenceContext;
import ch.eitchnet.xmlpers.api.XmlIoMode;
import ch.eitchnet.xmlpers.api.XmlPersistenceConstants;
import ch.eitchnet.xmlpers.impl.XmlPersistencePathBuilder;
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

	private FileDao fileDao;
	private Properties properties;

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
	}

	@Before
	public void setUp() {
		this.properties = new Properties();
		this.properties.setProperty(XmlPersistenceConstants.PROP_VERBOSE, "true");
	}

	@Test
	public void testCrudSax() {
		this.properties.setProperty(XmlPersistenceConstants.PROP_BASEPATH, TEST_PATH + "/sax/");
		XmlPersistencePathBuilder pathBuilder = new XmlPersistencePathBuilder(this.properties);
		this.fileDao = new FileDao(pathBuilder, VERBOSE);

		Resource resource = createResource();
		assertResource(resource);
		XmlIoMode ioMode = XmlIoMode.SAX;
		PersistenceContext<Resource> context = createPersistenceContext(resource, ioMode);
		testCrud(context);
	}

	@Test
	public void testCrudDom() {
		this.properties.setProperty(XmlPersistenceConstants.PROP_BASEPATH, TEST_PATH + "/dom/");
		XmlPersistencePathBuilder pathBuilder = new XmlPersistencePathBuilder(this.properties);
		this.fileDao = new FileDao(pathBuilder, VERBOSE);

		Resource resource = createResource();
		assertResource(resource);
		XmlIoMode ioMode = XmlIoMode.DOM;
		PersistenceContext<Resource> context = createPersistenceContext(resource, ioMode);
		testCrud(context);
	}

	private void testCrud(PersistenceContext<Resource> context) {

		this.fileDao.performCreate(context);

		context.setObject(null);
		this.fileDao.performRead(context);
		assertResource(context.getObject());

		updateResource(context.getObject());
		this.fileDao.performUpdate(context);

		context.setObject(null);
		this.fileDao.performRead(context);
		assertResourceUpdated(context.getObject());

		this.fileDao.performDelete(context);

		context.setObject(null);
		this.fileDao.performRead(context);
		assertNull(context.getObject());

		context.setObject(createResource());
		this.fileDao.performCreate(context);
	}

	private PersistenceContext<Resource> createPersistenceContext(Resource resource, XmlIoMode ioMode) {
		PersistenceContext<Resource> context = new PersistenceContext<Resource>();
		context.setId(resource.getId());
		context.setType(TestConstants.TYPE_RES);
		context.setSubType(resource.getType());
		context.setObject(resource);
		context.setIoMode(ioMode);
		context.setParserFactory(new ResourceParserFactory());

		return context;
	}
}
