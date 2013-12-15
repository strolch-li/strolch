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
package li.strolch.persistence.impl.dao.test;

import static li.strolch.model.ModelGenerator.BAG_ID;
import static li.strolch.model.ModelGenerator.PARAM_STRING_ID;
import static li.strolch.model.ModelGenerator.createResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import li.strolch.model.Resource;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public class XmlResourceDaoTest extends AbstractDaoImplTest {

	private static final String ID = "@testResource"; //$NON-NLS-1$
	private static final String NAME = "Test Resource"; //$NON-NLS-1$
	private static final String TYPE = "Box"; //$NON-NLS-1$

	@Test
	public void shouldCreateResource() {

		// create
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).save(newResource);
		}
	}

	@Test
	public void shouldCrud() {

		// create
		Resource newResource = createResource(ID, NAME, TYPE);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).save(newResource);
		}

		// read
		Resource readResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			readResource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, readResource); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).update(readResource);
		}

		// read updated
		Resource updatedResource = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			updatedResource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Resource with id " + ID, updatedResource); //$NON-NLS-1$
		assertFalse("Objects can't be the same reference after re-reading!", readResource == updatedResource); //$NON-NLS-1$
		Parameter<String> updatedParam = readResource.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).remove(readResource);
		}

		// fail to re-read
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			Resource resource = persistenceHandler.getResourceDao(tx).queryBy(TYPE, ID);
			assertNull("Should no read Resource with id " + ID, resource); //$NON-NLS-1$
		}
	}
}
