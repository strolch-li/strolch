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
package li.strolch.persistence.postgresql.dao.test;

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

import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public class PostgreSqlResourceDaoTest extends AbstractDaoImplTest {

	private static final String ID = "@testResource"; //$NON-NLS-1$
	private static final String NAME = "Test Resource"; //$NON-NLS-1$
	private static final String TYPE = "Box"; //$NON-NLS-1$

	@Test
	public void shouldCreateResource() {

		// create
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getResourceDao().save(newResource);
		}
	}

	@Test
	public void shouldCrud() {

		// create
		Resource newResource = createResource(ID, NAME, TYPE);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getResourceDao().save(newResource);
		}

		// read
		Resource readResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			readResource = tx.getResourceDao().queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, readResource); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getResourceDao().update(readResource);
		}

		// read updated
		Resource updatedResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			updatedResource = tx.getResourceDao().queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, updatedResource); //$NON-NLS-1$
		assertFalse("Objects can't be the same reference after re-reading!", readResource == updatedResource); //$NON-NLS-1$
		Parameter<String> updatedParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			tx.getResourceDao().remove(readResource);
		}

		// fail to re-read
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			Resource resource = tx.getResourceDao().queryBy(TYPE, ID);
			assertNull("Should no read Resource with id " + ID, resource); //$NON-NLS-1$
		}
	}

	@SuppressWarnings("nls")
	@Test
	public void shouldPerformBulkOperations() {

		List<Resource> resources = new ArrayList<>();
		resources.addAll(createResources(resources.size(), 5, "@", "My Resource ", "MyType1"));
		resources.addAll(createResources(resources.size(), 5, "@", "Other Resource ", "MyType2"));
		resources.addAll(createResources(resources.size(), 5, "@", "Further Resource ", "MyType3"));

		Comparator<Resource> comparator = new Comparator<Resource>() {
			@Override
			public int compare(Resource o1, Resource o2) {
				return o1.getId().compareTo(o2.getId());
			}
		};
		Collections.sort(resources, comparator);

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			tx.getResourceDao().removeAll(tx.getResourceDao().queryAll());
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			tx.getResourceDao().saveAll(resources);
		}

		Set<String> expectedTypes = new HashSet<>();
		expectedTypes.add("MyType1");
		expectedTypes.add("MyType2");
		expectedTypes.add("MyType3");

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			List<Resource> allResources = tx.getResourceDao().queryAll();
			Collections.sort(allResources, comparator);
			assertEquals(resources, allResources);
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			ResourceDao resourceDao = tx.getResourceDao();

			Set<String> types = resourceDao.queryTypes();
			assertEquals(expectedTypes, types);

			Set<String> keySet = resourceDao.queryKeySet();
			assertEquals(15, keySet.size());

			for (String type : types) {
				Set<String> idsByType = resourceDao.queryKeySet(type);
				assertEquals(5, idsByType.size());

				List<Resource> resourcesByType = resourceDao.queryAll(type);
				assertEquals(5, resourcesByType.size());
			}
		}

		try (StrolchTransaction tx = persistenceHandler.openTx()) {
			Resource resource = tx.getResourceDao().queryBy("MyType1", "@_1");
			assertNotNull(resource);
			resource = tx.getResourceDao().queryBy("MyType2", "@_6");
			assertNotNull(resource);
			resource = tx.getResourceDao().queryBy("MyType3", "@_11");
			assertNotNull(resource);
		}
	}
}
