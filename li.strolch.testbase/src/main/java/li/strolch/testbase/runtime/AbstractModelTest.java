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

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.privilege.PrivilegeHandler;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.model.Certificate;

public abstract class AbstractModelTest {

	protected static final Logger logger = LoggerFactory.getLogger(AbstractModelTest.class);

	protected abstract RuntimeMock getRuntimeMock();

	protected String realmName = StrolchConstants.DEFAULT_REALM;

	@Test
	public void shouldStartContainer() {
		PrivilegeHandler privilegeHandler = getRuntimeMock().getContainer().getPrivilegeHandler();
		Certificate certificate = privilegeHandler.authenticate("test", "test".getBytes());
		try (StrolchTransaction tx = getRuntimeMock().getRealm(this.realmName).openTx(certificate, "test")) {
			tx.getOrderMap().getAllKeys(tx);
		}
	}

	@Test
	public void shouldCreateOrders() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCreateOrderTest();
	}

	@Test
	public void shouldQueryOrderSizes() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runQuerySizeTest();
	}

	@Test
	public void shouldOrderCrud() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCrudTests();
	}

	@Test
	public void shouldOrderPerformBulkOperations() {

		OrderModelTestRunner testRunner = new OrderModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runBulkOperationTests();
	}

	@Test
	public void shouldCreateResources() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCreateResourceTest();
	}

	@Test
	public void shouldQueryResourceSizes() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runQuerySizeTest();
	}

	@Test
	public void shouldResourceCrud() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runCrudTests();
	}

	@Test
	public void shouldResourcePerformBulkOperations() {

		ResourceModelTestRunner testRunner = new ResourceModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runBulkOperationTests();
	}

	@Test
	public void shouldTestAudits() {
		AuditModelTestRunner testRunner = new AuditModelTestRunner(getRuntimeMock(), this.realmName);
		testRunner.runTestForAudits();
	}
}
