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

import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchBootstrapper;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.StrolchConstants;
import li.strolch.testbase.runtime.RuntimeMock;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static li.strolch.model.ModelGenerator.createOrder;
import static li.strolch.model.ModelGenerator.createResource;
import static org.junit.Assert.assertEquals;

public class CachedRealmLoaderTest {

	public static final String RUNTIME_PATH = "target/cachedRealmLoaderTestRuntime/";
	public static final String CONFIG_SRC = "src/test/resources/cachedRuntime";

	private RuntimeMock runtimeMock;

	@Before
	public void setUp() {
		File rootPath = new File(RUNTIME_PATH);
		File configSrc = new File(CONFIG_SRC);
		this.runtimeMock = new RuntimeMock();
		this.runtimeMock.mockRuntime(rootPath, configSrc);
		this.runtimeMock.startContainer();
	}

	@After
	public void tearDown() {
		if (this.runtimeMock != null)
			this.runtimeMock.destroyRuntime();
	}

	@Test
	public void testConcurrentRealmLoading() {
		Certificate certificate = this.runtimeMock.getPrivilegeHandler().authenticate("test", "test".toCharArray());
		StrolchRealm realm = this.runtimeMock.getRealm(StrolchConstants.DEFAULT_REALM);

		List<Resource> resources = new ArrayList<>();
		for (int i = 0; i < 10; i++) {
			resources.add(createResource("c_sm1_" + i, "Small 1 " + i, "ConcurrentSmallType1"));
			resources.add(createResource("c_sm2_" + i, "Small 2 " + i, "ConcurrentSmallType2"));
			resources.add(createResource("c_sm3_" + i, "Small 3 " + i, "ConcurrentSmallType3"));
		}
		for (int i = 0; i < 500; i++) {
			resources.add(createResource("c_lg1_" + i, "Large 1 " + i, "ConcurrentLargeType1"));
			resources.add(createResource("c_lg2_" + i, "Large 2 " + i, "ConcurrentLargeType2"));
		}

		List<Order> orders = new ArrayList<>();
		for (int i = 0; i < 15; i++) {
			orders.add(createOrder("c_ord_sm_" + i, "Order Small " + i, "ConcurrentSmallOrderType"));
		}
		for (int i = 0; i < 400; i++) {
			orders.add(createOrder("c_ord_lg_" + i, "Order Large " + i, "ConcurrentLargeOrderType"));
		}

		try (StrolchTransaction tx = realm.openTx(certificate, "test", false)) {
			ResourceDao resDao = tx.getPersistenceHandler().getResourceDao(tx);
			resDao.saveAll(resources);

			OrderDao orderDao = tx.getPersistenceHandler().getOrderDao(tx);
			orderDao.saveAll(orders);

			tx.commitOnClose();
		}

		// Restart container to trigger CachedRealmLoader concurrently from persisted XML
		this.runtimeMock.destroyRuntime();
		StrolchAgent agent = new StrolchBootstrapper(RuntimeMock.getAppVersion())
				.setupByRoot("dev", new File(RUNTIME_PATH));
		agent.initialize();
		agent.start();
		Certificate cert = agent.getContainer().getPrivilegeHandler().authenticate("test", "test".toCharArray());

		try {
			StrolchRealm restartedRealm = agent.getContainer().getRealm(StrolchConstants.DEFAULT_REALM);
			try (StrolchTransaction tx = restartedRealm.openTx(cert, "test", true)) {
				ResourceDao resDao = tx.getPersistenceHandler().getResourceDao(tx);
				assertEquals(10, resDao.querySize("ConcurrentSmallType1"));
				assertEquals(10, resDao.querySize("ConcurrentSmallType2"));
				assertEquals(10, resDao.querySize("ConcurrentSmallType3"));
				assertEquals(500, resDao.querySize("ConcurrentLargeType1"));
				assertEquals(500, resDao.querySize("ConcurrentLargeType2"));

				assertEquals(10, tx.getResourceMap().querySize(tx, "ConcurrentSmallType1"));
				assertEquals(10, tx.getResourceMap().querySize(tx, "ConcurrentSmallType2"));
				assertEquals(10, tx.getResourceMap().querySize(tx, "ConcurrentSmallType3"));
				assertEquals(500, tx.getResourceMap().querySize(tx, "ConcurrentLargeType1"));
				assertEquals(500, tx.getResourceMap().querySize(tx, "ConcurrentLargeType2"));

				OrderDao orderDao = tx.getPersistenceHandler().getOrderDao(tx);
				assertEquals(15, orderDao.querySize("ConcurrentSmallOrderType"));
				assertEquals(400, orderDao.querySize("ConcurrentLargeOrderType"));

				assertEquals(15, tx.getOrderMap().querySize(tx, "ConcurrentSmallOrderType"));
				assertEquals(400, tx.getOrderMap().querySize(tx, "ConcurrentLargeOrderType"));
			}
		} finally {
			agent.stop();
			agent.destroy();
		}
	}
}
