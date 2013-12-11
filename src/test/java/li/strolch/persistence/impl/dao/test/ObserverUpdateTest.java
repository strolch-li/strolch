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
package li.strolch.persistence.impl.dao.test;

import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static org.junit.Assert.assertEquals;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.observer.Observer;
import li.strolch.runtime.observer.ObserverHandler;

import org.junit.Test;

import ch.eitchnet.xmlpers.api.ModificationResult;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ObserverUpdateTest extends AbstractDaoImplTest {

	public final class ElementAddedObserver implements Observer {

		Map<String, ModificationResult> results = new HashMap<>();

		private ModificationResult getModificationResult(String key) {
			ModificationResult result = this.results.get(key);
			if (result == null) {
				result = new ModificationResult(key);
				this.results.put(key, result);
			}
			return result;
		}

		@Override
		public void update(String key, List<StrolchElement> elements) {
			getModificationResult(key).getUpdated().addAll(elements);
		}

		@Override
		public void remove(String key, List<StrolchElement> elements) {
			getModificationResult(key).getDeleted().addAll(elements);
		}

		@Override
		public void add(String key, List<StrolchElement> elements) {
			getModificationResult(key).getCreated().addAll(elements);
		}
	}

	@Test
	public void shouldReceiveUpdates() {

		// register an observer for orders and resources
		ElementAddedObserver observer = new ElementAddedObserver();
		getContainer().getComponent(ObserverHandler.class).registerObserver("Order", observer); //$NON-NLS-1$
		getContainer().getComponent(ObserverHandler.class).registerObserver("Resource", observer); //$NON-NLS-1$

		// create order
		Order newOrder = createOrder("MyTestOrder", "Test Name", "TestType", new Date(), State.CREATED); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getOrderDao(tx).save(newOrder);
		}

		// create resource
		Resource newResource = createResource("MyTestResource", "Test Name", "TestType"); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		try (StrolchTransaction tx = persistenceHandler.openTx();) {
			persistenceHandler.getResourceDao(tx).save(newResource);
		}

		assertEquals(2, observer.results.size());
		assertEquals(1, observer.results.get("Order").getCreated().size()); //$NON-NLS-1$
		assertEquals(1, observer.results.get("Resource").getCreated().size()); //$NON-NLS-1$

	}
}
