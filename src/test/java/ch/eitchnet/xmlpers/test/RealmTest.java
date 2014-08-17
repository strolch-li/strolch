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
package ch.eitchnet.xmlpers.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Properties;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import ch.eitchnet.xmlpers.api.IoMode;
import ch.eitchnet.xmlpers.api.PersistenceConstants;
import ch.eitchnet.xmlpers.api.PersistenceTransaction;
import ch.eitchnet.xmlpers.objref.ObjectRef;
import ch.eitchnet.xmlpers.test.impl.TestConstants;
import ch.eitchnet.xmlpers.test.model.ModelBuilder;
import ch.eitchnet.xmlpers.test.model.MyModel;

@SuppressWarnings("nls")
public class RealmTest extends AbstractPersistenceTest {

	private static final String REALM_2 = "Realm2";
	private static final String REALM_1 = "Realm1";
	protected static final String BASE_PATH = "target/db/RealmTest/";

	@BeforeClass
	public static void beforeClass() {
		cleanPath(BASE_PATH);
	}

	@Before
	public void before() {
		Properties properties = new Properties();
		properties.setProperty(PersistenceConstants.PROP_BASEPATH, BASE_PATH + IoMode.DOM.name());
		setup(properties);
	}

	@Test
	public void shouldNotFindObjInBothRealms() {

		// object details
		String objType = TestConstants.TYPE_RES;
		String type = ModelBuilder.RES_TYPE;
		String name = ModelBuilder.RES_NAME;
		String id = "shouldNotFindObjInBothRealms";

		// create in first realm
		try (PersistenceTransaction txRealm1 = this.persistenceManager.openTx(REALM_1);) {
			MyModel resource1 = ModelBuilder.createResource(id, name, type);
			txRealm1.getObjectDao().add(resource1);
		}

		// find in first realm
		try (PersistenceTransaction txRealm1 = this.persistenceManager.openTx(REALM_1);) {
			ObjectRef objectRef = txRealm1.getObjectRefCache().getIdOfSubTypeRef(objType, type, id);
			MyModel resource = txRealm1.getObjectDao().queryById(objectRef);
			assertNotNull("Resource was not found in first realm!", resource);
		}

		// fail to find in second realm
		try (PersistenceTransaction txRealm2 = this.persistenceManager.openTx(REALM_2);) {
			ObjectRef objectRef = txRealm2.getObjectRefCache().getIdOfSubTypeRef(objType, type, id);
			MyModel resource = txRealm2.getObjectDao().queryById(objectRef);
			assertNull("Resource was not created in second realm, thus not expected to be found there!", resource);
		}
	}

	@Test
	public void shouldNotDeleteObjInWrongRealm() {

		// object details
		String objType = TestConstants.TYPE_RES;
		String subType = ModelBuilder.RES_TYPE;
		String name = ModelBuilder.RES_NAME;
		String id = "shouldNotDeleteObjInWrongRealm";

		// create in first realm
		try (PersistenceTransaction txRealm1 = this.persistenceManager.openTx(REALM_1);) {
			MyModel resource1 = ModelBuilder.createResource(id, name, subType);
			txRealm1.getObjectDao().add(resource1);
		}

		// create in second realm
		try (PersistenceTransaction txRealm2 = this.persistenceManager.openTx(REALM_2);) {
			MyModel resource1 = ModelBuilder.createResource(id, name, subType);
			txRealm2.getObjectDao().add(resource1);
		}

		// delete in second realm
		try (PersistenceTransaction txRealm2 = this.persistenceManager.openTx(REALM_2);) {
			ObjectRef objectRef = txRealm2.getObjectRefCache().getIdOfSubTypeRef(objType, subType, id);
			txRealm2.getObjectDao().removeById(objectRef);
		}

		// fail to find in second realm
		try (PersistenceTransaction txRealm2 = this.persistenceManager.openTx(REALM_2);) {
			ObjectRef objectRef = txRealm2.getObjectRefCache().getIdOfSubTypeRef(objType, subType, id);
			MyModel resource = txRealm2.getObjectDao().queryById(objectRef);
			assertNull("Resource was not deleted from second realm, thus not expected to be found there!", resource);
		}

		// find in first realm
		try (PersistenceTransaction txRealm1 = this.persistenceManager.openTx(REALM_1);) {
			ObjectRef objectRef = txRealm1.getObjectRefCache().getIdOfSubTypeRef(objType, subType, id);
			MyModel resource = txRealm1.getObjectDao().queryById(objectRef);
			assertNotNull("Resource was not found in first realm!", resource);
		}
	}
}
