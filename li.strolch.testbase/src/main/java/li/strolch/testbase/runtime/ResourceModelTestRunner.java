/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.testbase.runtime;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;
import static li.strolch.model.ModelGenerator.createResource;
import static li.strolch.model.ModelGenerator.createResources;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.privilege.PrivilegeHandler;
import ch.eitchnet.privilege.model.Certificate;

@SuppressWarnings("nls")
public class ResourceModelTestRunner {

	private static final String ID = "@testResource"; //$NON-NLS-1$
	private static final String NAME = "Test Resource"; //$NON-NLS-1$
	private static final String TYPE = "Box"; //$NON-NLS-1$

	private RuntimeMock runtimeMock;
	private String realmName;
	private Certificate certificate;

	public ResourceModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".getBytes());
	}

	public void runCreateResourceTest() {

		// create
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().add(tx, newResource);
			tx.commitOnClose();
		}
	}

	public void runQuerySizeTest() {

		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().removeAll(tx, tx.getResourceMap().getAllElements(tx));
			tx.commitOnClose();
		}

		// create three resources
		Resource resource1 = createResource("myTestResource1", "Test Name", "QTestType1"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Resource resource2 = createResource("myTestResource2", "Test Name", "QTestType2"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		Resource resource3 = createResource("myTestResource3", "Test Name", "QTestType3"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().add(tx, resource1);
			tx.getResourceMap().add(tx, resource2);
			tx.getResourceMap().add(tx, resource3);
			tx.commitOnClose();
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			long size = tx.getResourceMap().querySize(tx);
			assertEquals("Should have three objects", 3, size);

			size = tx.getResourceMap().querySize(tx, "QTestType1");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getResourceMap().querySize(tx, "QTestType2");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getResourceMap().querySize(tx, "QTestType3");
			assertEquals("Should have only one object of type 'QTestType1'", 1, size);

			size = tx.getResourceMap().querySize(tx, "NonExistingType");
			assertEquals("Should have zero objects of type 'NonExistingType'", 0, size);
		}
	}

	public void runCrudTests() {

		// create
		Resource newResource = createResource(ID, NAME, TYPE);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().add(tx, newResource);
			tx.commitOnClose();
		}

		// read
		Resource readResource = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			readResource = tx.getResourceMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, readResource); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().update(tx, readResource);
			tx.commitOnClose();
		}

		// read updated
		Resource updatedResource = null;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			updatedResource = tx.getResourceMap().getBy(tx, TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, updatedResource); //$NON-NLS-1$
		if (this.runtimeMock.getRealm(this.realmName).getMode() != DataStoreMode.CACHED)
			assertFalse("Objects can't be the same reference after re-reading!", readResource == updatedResource); //$NON-NLS-1$
		Parameter<String> updatedParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			tx.getResourceMap().remove(tx, readResource);
			tx.commitOnClose();
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test");) {
			Resource resource = tx.getResourceMap().getBy(tx, TYPE, ID);
			assertNull("Should no read Resource with id " + ID, resource); //$NON-NLS-1$
		}
	}

	public void runBulkOperationTests() {

		// create 15 resources
		List<Resource> resources = new ArrayList<>();
		resources.addAll(createResources(resources.size(), 5, "@", "My Resource", "MyType1"));
		resources.addAll(createResources(resources.size(), 5, "@", "Other Resource", "MyType2"));
		resources.addAll(createResources(resources.size(), 5, "@", "Further Resource", "MyType3"));

		// sort them so we know which order our objects are
		Comparator<Resource> comparator = new Comparator<Resource>() {
			@Override
			public int compare(Resource o1, Resource o2) {
				return o1.getId().compareTo(o2.getId());
			}
		};
		Collections.sort(resources, comparator);

		// first clear the map, so that we have a clean state
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();
			resourceMap.removeAll(tx, resourceMap.getAllElements(tx));
			tx.commitOnClose();
		}

		{
			// make sure it is empty
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(0, resourceMap.querySize(tx));
			}

			// now add some resources
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				tx.getResourceMap().addAll(tx, resources);
				tx.commitOnClose();
			}

			// make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(resources.size(), resourceMap.querySize(tx));
				assertEquals(5, resourceMap.querySize(tx, "MyType3"));
			}

			// now use the remove all by type
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				tx.getResourceMap().removeAllBy(tx, "MyType3");
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(resources.size() - 5, resourceMap.querySize(tx));
				assertEquals(0, resourceMap.querySize(tx, "MyType3"));
			}

			// now use the remove all
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				long removed = tx.getResourceMap().removeAll(tx);
				assertEquals(resources.size() - 5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(0, resourceMap.querySize(tx));
			}
		}

		// now add all again
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			tx.getResourceMap().addAll(tx, resources);
			tx.commitOnClose();
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			List<Resource> allResources = tx.getResourceMap().getAllElements(tx);
			Collections.sort(allResources, comparator);
			assertEquals(resources, allResources);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			ResourceMap resourceMap = tx.getResourceMap();

			Set<String> types = resourceMap.getTypes(tx);
			assertEquals(expectedTypes, types);

			Set<String> keySet = resourceMap.getAllKeys(tx);
			assertEquals(15, keySet.size());

			for (String type : types) {
				Set<String> idsByType = resourceMap.getKeysBy(tx, type);
				assertEquals(5, idsByType.size());

				List<Resource> resourcesByType = resourceMap.getElementsBy(tx, type);
				assertEquals(5, resourcesByType.size());
			}
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test")) {
			Resource resource = tx.getResourceMap().getBy(tx, "MyType1", "@00000001");
			assertNotNull(resource);
			resource = tx.getResourceMap().getBy(tx, "MyType2", "@00000006");
			assertNotNull(resource);
			resource = tx.getResourceMap().getBy(tx, "MyType3", "@00000011");
			assertNotNull(resource);
		}
	}
}
