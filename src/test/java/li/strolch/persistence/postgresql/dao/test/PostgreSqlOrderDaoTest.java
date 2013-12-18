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
import static li.strolch.model.ModelGenerator.createOrder;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import li.strolch.model.Order;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

import org.junit.Test;

public class PostgreSqlOrderDaoTest extends AbstractDaoImplTest {

	private static final String ID = "@testOrder"; //$NON-NLS-1$
	private static final String NAME = "Test Order"; //$NON-NLS-1$
	private static final String TYPE = "ToStock"; //$NON-NLS-1$

	@Test
	public void shouldCreateOrder() {

		// create
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getOrderDao(tx).save(newOrder);
		}
	}

	@Test
	public void shouldCrud() {

		// create
		Order newOrder = createOrder(ID, NAME, TYPE);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getOrderDao(tx).save(newOrder);
		}

		// read
		Order readOrder = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			readOrder = persistenceHandler.getOrderDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, readOrder); //$NON-NLS-1$

		// update
		Parameter<String> sParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		String newStringValue = "Giddiya!"; //$NON-NLS-1$
		sParam.setValue(newStringValue);
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getOrderDao(tx).update(readOrder);
		}

		// read updated
		Order updatedOrder = null;
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			updatedOrder = persistenceHandler.getOrderDao(tx).queryBy(TYPE, ID);
		}
		assertNotNull("Should read Order with id " + ID, updatedOrder); //$NON-NLS-1$
		assertFalse("Objects can't be the same reference after re-reading!", readOrder == updatedOrder); //$NON-NLS-1$
		Parameter<String> updatedParam = readOrder.getParameter(BAG_ID, PARAM_STRING_ID);
		assertEquals(newStringValue, updatedParam.getValue());

		// delete
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getOrderDao(tx).remove(readOrder);
		}

		// fail to re-read
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			Order order = persistenceHandler.getOrderDao(tx).queryBy(TYPE, ID);
			assertNull("Should no read Order with id " + ID, order); //$NON-NLS-1$
		}
	}
}
