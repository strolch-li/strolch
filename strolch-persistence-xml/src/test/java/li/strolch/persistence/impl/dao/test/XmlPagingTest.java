/*
 * Copyright (c) 2013-2026 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;
import li.strolch.utils.collections.DateRange;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.ResourceBundle;

import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static org.junit.Assert.*;

public class XmlPagingTest {

	public static final String RUNTIME_PATH = "target/pagingStrolchRuntime/";
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime";

	protected static RuntimeMock runtimeMock;
	protected static Certificate certificate;

	@BeforeClass
	public static void beforeClass() {
		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		runtimeMock = new RuntimeMock();
		runtimeMock.mockRuntime(rootPath, configSrc);
		runtimeMock.startContainer();
		certificate = runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
	}

	@AfterClass
	public static void afterClass() {
		if (runtimeMock != null)
			runtimeMock.destroyRuntime();
	}

	@Test
	public void testSupportsPaging() {
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			assertTrue(tx.getPersistenceHandler().supportsPaging());
			assertTrue(tx.getPersistenceHandler().getResourceDao(tx).supportsPaging());
			assertTrue(tx.getPersistenceHandler().getOrderDao(tx).supportsPaging());
			assertTrue(tx.getPersistenceHandler().getActivityDao(tx).supportsPaging());
			assertTrue(tx.getPersistenceHandler().getLogMessageDao(tx).supportsPaging());
		}
	}

	@Test
	public void testResourcePaging() {
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);

		// Add resources of 2 types
		List<Resource> typeAResources = new ArrayList<>();
		List<Resource> typeBResources = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			typeAResources.add(createResource("p_res_a_" + i, "Name A " + i, "PagingTypeA"));
			typeBResources.add(createResource("p_res_b_" + i, "Name B " + i, "PagingTypeB"));
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			ResourceDao dao = tx.getPersistenceHandler().getResourceDao(tx);
			dao.saveAll(typeAResources);
			dao.saveAll(typeBResources);
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			ResourceDao dao = tx.getPersistenceHandler().getResourceDao(tx);

			// Single type paging
			List<Resource> page1 = dao.queryAll(3, 0, "PagingTypeA");
			assertEquals(3, page1.size());
			assertEquals("p_res_a_0", page1.get(0).getId());
			assertEquals("p_res_a_1", page1.get(1).getId());
			assertEquals("p_res_a_2", page1.get(2).getId());

			List<Resource> page2 = dao.queryAll(3, 3, "PagingTypeA");
			assertEquals(2, page2.size());
			assertEquals("p_res_a_3", page2.get(0).getId());
			assertEquals("p_res_a_4", page2.get(1).getId());

			List<Resource> pageEmpty = dao.queryAll(3, 5, "PagingTypeA");
			assertEquals(0, pageEmpty.size());

			// Multiple types paging
			List<Resource> multiPage = dao.queryAll(4, 2, "PagingTypeA", "PagingTypeB");
			assertEquals(4, multiPage.size());

			// Global paging
			List<Resource> allPage = dao.queryAll(5, 0);
			assertEquals(5, allPage.size());
		}

		// Cleanup
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			ResourceDao dao = tx.getPersistenceHandler().getResourceDao(tx);
			dao.removeAll(typeAResources);
			dao.removeAll(typeBResources);
			tx.commitOnClose();
		}
	}

	@Test
	public void testOrderDateRangePaging() {
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);

		List<Order> orders = new ArrayList<>();
		long baseTime = System.currentTimeMillis() - 100000;
		for (int i = 0; i < 6; i++) {
			Order order = createOrder("p_ord_" + i, "Order " + i, "PagingOrderType");
			order.setDate(new Date(baseTime + (i * 10000)));
			orders.add(order);
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			OrderDao dao = tx.getPersistenceHandler().getOrderDao(tx);
			dao.saveAll(orders);
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			OrderDao dao = tx.getPersistenceHandler().getOrderDao(tx);
			DateRange dateRange = new DateRange().from(new Date(baseTime - 1000), true).to(new Date(baseTime + 70000), true);

			// Ascending
			List<Order> ascPage1 = dao.queryAll(dateRange, 3, 0, true, "PagingOrderType");
			assertEquals(3, ascPage1.size());
			assertEquals("p_ord_0", ascPage1.get(0).getId());
			assertEquals("p_ord_1", ascPage1.get(1).getId());
			assertEquals("p_ord_2", ascPage1.get(2).getId());

			List<Order> ascPage2 = dao.queryAll(dateRange, 3, 3, true, "PagingOrderType");
			assertEquals(3, ascPage2.size());
			assertEquals("p_ord_3", ascPage2.get(0).getId());
			assertEquals("p_ord_4", ascPage2.get(1).getId());
			assertEquals("p_ord_5", ascPage2.get(2).getId());

			// Descending
			List<Order> descPage1 = dao.queryAll(dateRange, 3, 0, false, "PagingOrderType");
			assertEquals(3, descPage1.size());
			assertEquals("p_ord_5", descPage1.get(0).getId());
			assertEquals("p_ord_4", descPage1.get(1).getId());
			assertEquals("p_ord_3", descPage1.get(2).getId());
		}

		// Cleanup
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			OrderDao dao = tx.getPersistenceHandler().getOrderDao(tx);
			dao.removeAll(orders);
			tx.commitOnClose();
		}
	}

	@Test
	public void testLogMessagePaging() {
		StrolchRealm realm = runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);
		ResourceBundle bundle = ResourceBundle.getBundle("li-strolch-testbase");

		List<LogMessage> messages = new ArrayList<>();
		ZonedDateTime now = ZonedDateTime.now();
		for (int i = 0; i < 5; i++) {
			LogMessage logMessage = new LogMessage("msg_" + i, now.plusSeconds(i), StrolchConstants.DEFAULT_REALM,
					SYSTEM_USER_AGENT, Locator.valueOf(AGENT, "li.strolch.testbase", "XmlPagingTest"),
					LogSeverity.Info, LogMessageState.Information, bundle.getBaseBundleName(), "test-message",
					java.util.Collections.emptyMap(), "test-message-" + i, null);
			messages.add(logMessage);
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			LogMessageDao dao = tx.getPersistenceHandler().getLogMessageDao(tx);
			dao.saveAll(messages);
			tx.commitOnClose();
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", true)) {
			LogMessageDao dao = tx.getPersistenceHandler().getLogMessageDao(tx);

			List<LogMessage> page1 = dao.queryLatest(StrolchConstants.DEFAULT_REALM, 2, 0);
			assertEquals(2, page1.size());

			List<LogMessage> page2 = dao.queryLatest(StrolchConstants.DEFAULT_REALM, 2, 2);
			assertEquals(2, page2.size());
			assertNotEquals(page1.get(0).getId(), page2.get(0).getId());
		}

		// Cleanup
		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			LogMessageDao dao = tx.getPersistenceHandler().getLogMessageDao(tx);
			dao.removeAll(messages);
			tx.commitOnClose();
		}
	}
}
