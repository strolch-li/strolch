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

import static li.strolch.model.ModelGenerator.*;
import static org.junit.Assert.*;

import java.util.*;

import li.strolch.agent.api.ResourceMap;
import li.strolch.agent.impl.DataStoreMode;
import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

@SuppressWarnings("nls")
public class ResourceModelTestRunner {

	private static final String ID = "@testResource";
	private static final String NAME = "Test Resource";
	private static final String TYPE = "Box";

	private final RuntimeMock runtimeMock;
	private final String realmName;
	private final Certificate certificate;

	public ResourceModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runCreateResourceTest() {

		// create
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType");//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newResource);
			tx.commitOnClose();
		}
	}

	public void runQuerySizeTest() {

		// remove all
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getResourceMap().removeAll(tx, tx.getResourceMap().getAllElements(tx));
			tx.commitOnClose();
		}

		// create three resources
		Resource resource1 = createResource("myTestResource1", "Test Name", "QTestType1");//$NON-NLS-2$ //$NON-NLS-3$
		Resource resource2 = createResource("myTestResource2", "Test Name", "QTestType2");//$NON-NLS-2$ //$NON-NLS-3$
		Resource resource3 = createResource("myTestResource3", "Test Name", "QTestType3");//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(resource1);
			tx.add(resource2);
			tx.add(resource3);
			tx.commitOnClose();
		}

		// query size
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
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
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.add(newResource);
			tx.commitOnClose();
		}

		// read
		Resource readResource;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			readResource = tx.getResourceBy(TYPE, ID, true).getClone(true);
		}

		// update
		StringParameter sParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!";
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getResourceMap().update(tx, readResource);
			tx.commitOnClose();
		}

		// read updated
		Resource updatedResource;
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			updatedResource = tx.getResourceBy(TYPE, ID, true);
		}
		assertNotNull("Should read Resource with id " + ID, updatedResource);
		if (this.runtimeMock.getRealm(this.realmName).getMode() != DataStoreMode.CACHED)
			assertNotSame("Objects can't be the same reference after re-reading!", readResource, updatedResource);
		StringParameter updatedParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getResourceMap().remove(tx, readResource);
			tx.commitOnClose();
		}

		// fail to re-read
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			Resource resource = tx.getResourceBy(TYPE, ID);
			assertNull("Should not read Resource with id " + ID, resource);
		}


		// create with same ID, but different types
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			Resource res = createResource("non-unique-id", "NonUnique1", "NonUnique1");
			tx.add(res);
			tx.commitOnClose();
		}
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			Resource res = createResource("non-unique-id", "NonUnique2", "NonUnique2");
			tx.add(res);
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			Resource res1 = tx.getResourceBy("NonUnique1", "non-unique-id");
			Resource res2 = tx.getResourceBy("NonUnique2", "non-unique-id");
			assertNotNull(res1);
			assertNotNull(res2);
			assertEquals("NonUnique1", res1.getName());
			assertEquals("NonUnique2", res2.getName());
		}
	}

	public void runBulkOperationTests() {

		// create 15 resources
		List<Resource> resources = new ArrayList<>();
		resources.addAll(createResources(0, 5, "@", "My Resource", "MyType1"));
		resources.addAll(createResources(resources.size(), 5, "@", "Other Resource", "MyType2"));
		resources.addAll(createResources(resources.size(), 5, "@", "Further Resource", "MyType3"));

		// sort them so we know which order our objects are
		resources.sort(Comparator.comparing(StrolchElement::getId));

		// first clear the map, so that we have a clean state
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			ResourceMap resourceMap = tx.getResourceMap();
			List<Resource> allElements = resourceMap.getAllElements(tx);
			long removed = resourceMap.removeAll(tx);
			assertEquals(allElements.size(), removed);
			assertEquals(0, resourceMap.querySize(tx));
			tx.commitOnClose();
		}

		{
			// make sure it is empty
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(0, resourceMap.querySize(tx));
			}

			// now add some resources
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				tx.getResourceMap().addAll(tx, resources);
				tx.commitOnClose();
			}

			// make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(resources.size(), resourceMap.querySize(tx));
				assertEquals(5, resourceMap.querySize(tx, "MyType3"));
			}

			// now use the remove all by type
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				long removed = tx.getResourceMap().removeAllBy(tx, "MyType3");
				assertEquals(5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(resources.size() - 5, resourceMap.querySize(tx));
				assertEquals(0, resourceMap.querySize(tx, "MyType3"));
			}

			// now use the remove all
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", false)) {
				long removed = tx.getResourceMap().removeAll(tx);
				assertEquals(resources.size() - 5, removed);
				tx.commitOnClose();
			}

			// again make sure we have our expected size
			try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
					.openTx(this.certificate, "test", true)) {
				ResourceMap resourceMap = tx.getResourceMap();
				assertEquals(0, resourceMap.querySize(tx));
			}
		}

		resources.forEach(t -> t.setVersion(null));

		// now add all again
		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName)
				.openTx(this.certificate, "test", false)) {
			tx.getResourceMap().addAll(tx, resources);
			tx.commitOnClose();
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			List<Resource> allResources = tx.getResourceMap().getAllElements(tx);
			allResources.sort(Comparator.comparing(StrolchElement::getId));
			assertEquals(resources, allResources);
		}

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
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

		try (StrolchTransaction tx = this.runtimeMock.getRealm(this.realmName).openTx(this.certificate, "test", true)) {
			Resource resource = tx.getResourceBy("MyType1", "@00000001");
			assertNotNull(resource);
			resource = tx.getResourceBy("MyType2", "@00000006");
			assertNotNull(resource);
			resource = tx.getResourceBy("MyType3", "@00000011");
			assertNotNull(resource);
		}
	}
}
