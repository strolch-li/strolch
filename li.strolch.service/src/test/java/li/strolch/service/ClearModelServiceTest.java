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
package li.strolch.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.test.AbstractRealmServiceTest;

import org.junit.Test;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearModelServiceTest extends AbstractRealmServiceTest {

	@Test
	public void runClearTest() {

		Runner validator = (strolchRealm, container) -> {
			try (StrolchTransaction tx = strolchRealm.openTx(ClearModelServiceTest.this.certificate, "test")) {
				assertEquals(0, tx.getResourceMap().querySize(tx));
				assertEquals(0, tx.getOrderMap().querySize(tx));
			}
		};

		ClearModelArgument arg = new ClearModelArgument();
		arg.clearOrders = true;
		arg.clearResources = true;

		runServiceInAllRealmTypes(ClearModelService.class, arg, null, validator, null);
	}

	@Test
	public void runClearOnlyOrdersTest() {

		Runner validator = (strolchRealm, container) -> {
			try (StrolchTransaction tx = strolchRealm.openTx(ClearModelServiceTest.this.certificate, "test")) {
				assertNotEquals(0, tx.getResourceMap().querySize(tx));
				assertEquals(0, tx.getOrderMap().querySize(tx));
			}
		};

		ClearModelArgument arg = new ClearModelArgument();
		arg.clearOrders = true;
		arg.clearResources = false;

		runServiceInAllRealmTypes(ClearModelService.class, arg, null, validator, null);
	}

	@Test
	public void runClearOnlyResourcesTest() {

		Runner validator = (strolchRealm, container) -> {
			try (StrolchTransaction tx = strolchRealm.openTx(ClearModelServiceTest.this.certificate, "test")) {
				assertNotEquals(0, tx.getOrderMap().querySize(tx));
				assertEquals(0, tx.getResourceMap().querySize(tx));
			}
		};

		ClearModelArgument arg = new ClearModelArgument();
		arg.clearOrders = false;
		arg.clearResources = true;

		runServiceInAllRealmTypes(ClearModelService.class, arg, null, validator, null);
	}
}
