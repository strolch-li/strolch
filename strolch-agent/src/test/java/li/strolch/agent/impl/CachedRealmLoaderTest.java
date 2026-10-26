/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.agent.impl;

import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Resource;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.OrderDao;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.ResourceDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Privilege;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.PasswordCrypt;
import org.junit.Test;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.time.ZonedDateTime;
import java.util.AbstractList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class CachedRealmLoaderTest {

	@Test
	public void loadsNonPagingTypesWithOneTransactionAtATimeAndInOrder() {
		assertLoadsWithOneTransactionAtATime(false);
	}

	@Test
	public void loadsPagingTypesInParallelWithinCpuLimitAndInOrder() {
		assertLoadsWithOneTransactionAtATime(true);
	}

	@Test
	public void pagesLargeTypesInParallelAndCompletesEveryOffset() {
		int availableProcessors = Runtime.getRuntime().availableProcessors();
		long resourceCount = CachedRealmLoader.MIN_PAGE_SIZE * availableProcessors + 1;
		long pageSize = Math.clamp(resourceCount / (availableProcessors * 2L), CachedRealmLoader.MIN_PAGE_SIZE,
				CachedRealmLoader.MAX_PAGE_SIZE);
		int expectedPages = (int) ((resourceCount + pageSize - 1) / pageSize);
		TransactionTracker tracker = new TransactionTracker();
		TestPersistenceHandler persistenceHandler = new TestPersistenceHandler(true, tracker, null, resourceCount);
		TestCachedRealm realm = new TestCachedRealm(tracker);

		new CachedRealmLoader(realm, persistenceHandler, createPrivilegeContext()).load("test");

		assertTrue(tracker.maxOpenTransactions.get() <= availableProcessors);
		if (availableProcessors > 1)
			assertTrue("Expected overlapping page loads", tracker.maxConcurrentElementQueries.get() > 1);
		assertEquals(List.of("Resources", "Orders", "Activities"), tracker.queriedCategories);
		assertEquals(expectedPages + 2, tracker.elementQueries.get());
		assertEquals(resourceCount, tracker.cachedResources.get());
		assertEquals(expectedPages, tracker.resourcePageOffsets.size());
		tracker.resourcePageOffsets.sort(Long::compareTo);
		for (int page = 0; page < expectedPages; page++)
			assertEquals((long) page * pageSize, tracker.resourcePageOffsets.get(page).longValue());
		assertEquals(0, tracker.categoryBarrierViolations.get());
		assertEquals(0, tracker.openTransactions.get());
	}

	@Test
	public void pagingHonorsSingleConnectionBudget() {
		int availableProcessors = Runtime.getRuntime().availableProcessors();
		long resourceCount = CachedRealmLoader.MIN_PAGE_SIZE * availableProcessors + 1;
		TransactionTracker tracker = new TransactionTracker(1);
		TestPersistenceHandler persistenceHandler = new TestPersistenceHandler(true, tracker, null, resourceCount);
		TestCachedRealm realm = new TestCachedRealm(tracker);

		new CachedRealmLoader(realm, persistenceHandler, createPrivilegeContext()).load("test");

		assertEquals(1, tracker.maxOpenTransactions.get());
		assertEquals(resourceCount, tracker.cachedResources.get());
		assertEquals(0, tracker.openTransactions.get());
	}

	@Test
	public void failedPageClosesTransactionsAndStopsLaterCategories() {
		int availableProcessors = Runtime.getRuntime().availableProcessors();
		long resourceCount = CachedRealmLoader.MIN_PAGE_SIZE * availableProcessors + 1;
		TransactionTracker tracker = new TransactionTracker();
		TestPersistenceHandler persistenceHandler = new TestPersistenceHandler(true, tracker, null, resourceCount,
				"Resources");
		TestCachedRealm realm = new TestCachedRealm(tracker);
		try {
			new CachedRealmLoader(realm, persistenceHandler, createPrivilegeContext()).load("test");
			throw new AssertionError("Expected loading to fail");
		} catch (IllegalStateException expected) {
			assertEquals("Failed to load realm test", expected.getMessage());
		}

		assertEquals(List.of("Resources"), tracker.queriedCategories);
		assertTrue(tracker.elementQueries.get() > 0);
		assertEquals(0, tracker.openTransactions.get());
	}

	@Test
	public void closesFailedTransactionAndStopsLoadingLaterCategories() {
		TransactionTracker tracker = new TransactionTracker();
		TestPersistenceHandler persistenceHandler = new TestPersistenceHandler(false, tracker, "Resources");
		TestCachedRealm realm = new TestCachedRealm(tracker);
		try {
			new CachedRealmLoader(realm, persistenceHandler, createPrivilegeContext()).load("test");
			throw new AssertionError("Expected loading to fail");
		} catch (IllegalStateException expected) {
			assertEquals("Failed to load realm test", expected.getMessage());
		}

		assertEquals(1, tracker.maxOpenTransactions.get());
		assertEquals(List.of("Resources"), tracker.queriedCategories);
		assertEquals(0, tracker.openTransactions.get());
		assertEquals(0, tracker.elementQueries.get());
	}

	private void assertLoadsWithOneTransactionAtATime(boolean supportsPaging) {
		TransactionTracker tracker = new TransactionTracker();
		TestPersistenceHandler persistenceHandler = new TestPersistenceHandler(supportsPaging, tracker, null);
		TestCachedRealm realm = new TestCachedRealm(tracker);
		new CachedRealmLoader(realm, persistenceHandler, createPrivilegeContext()).load("test");

		int expectedMaxTransactions = supportsPaging ? Math.min(Runtime.getRuntime().availableProcessors(), 2) : 1;
		assertEquals(expectedMaxTransactions, tracker.maxOpenTransactions.get());
		assertTrue(tracker.maxOpenTransactions.get() <= Runtime.getRuntime().availableProcessors());
		assertEquals(List.of("Resources", "Orders", "Activities"), tracker.queriedCategories);
		assertEquals(4, tracker.elementQueries.get());
		if (supportsPaging && Runtime.getRuntime().availableProcessors() > 1)
			assertTrue("Expected overlapping type loads", tracker.maxConcurrentElementQueries.get() > 1);
		assertEquals(0, tracker.categoryBarrierViolations.get());
		assertEquals(2, tracker.cachedResources.get());
		assertTrue(realm.getResourceMap().hasElement(null, "ResourcesTypeA", "ResourcesTypeAId"));
		assertTrue(realm.getResourceMap().hasElement(null, "ResourcesTypeB", "ResourcesTypeBId"));
		assertEquals(0, tracker.openTransactions.get());
	}

	private PrivilegeContext createPrivilegeContext() {
		Certificate certificate = new Certificate(Usage.ANY, "session", "user", "test", "Test", "User",
				UserState.ENABLED, PasswordCrypt.of(new byte[0], new byte[0]), "test", ZonedDateTime.now(), false,
				Locale.getDefault(), Set.of(), Set.of(), Map.of());
		return new PrivilegeContext(certificate, Map.<String, Privilege>of(), Map.of());
	}

	private static class TestCachedRealm extends CachedRealm {

		private final TransactionTracker tracker;
		private final CachedResourceMap resourceMap;
		private final CachedOrderMap orderMap = new CachedOrderMap(this);
		private final CachedActivityMap activityMap = new CachedActivityMap(this);

		private TestCachedRealm(TransactionTracker tracker) {
			super("test");
			this.tracker = tracker;
			this.resourceMap = new TrackingResourceMap(this, tracker);
		}

		@Override
		public StrolchTransaction openTx(Certificate certificate, String action, boolean readOnly) {
			return this.tracker.openTransaction();
		}

		@Override
		public CachedResourceMap getResourceMap() {
			return this.resourceMap;
		}

		@Override
		public CachedOrderMap getOrderMap() {
			return this.orderMap;
		}

		@Override
		public CachedActivityMap getActivityMap() {
			return this.activityMap;
		}
	}

	private static class TestPersistenceHandler implements PersistenceHandler {

		private final boolean supportsPaging;
		private final ResourceDao resourceDao;
		private final OrderDao orderDao;
		private final ActivityDao activityDao;

		private TestPersistenceHandler(boolean supportsPaging, TransactionTracker tracker, String failedCategory) {
			this(supportsPaging, tracker, failedCategory, 2);
		}

		private TestPersistenceHandler(boolean supportsPaging, TransactionTracker tracker, String failedCategory,
				long resourceCount) {
			this(supportsPaging, tracker, failedCategory, resourceCount, null);
		}

		private TestPersistenceHandler(boolean supportsPaging, TransactionTracker tracker, String failedCategory,
				long resourceCount, String failedPageCategory) {
			this.supportsPaging = supportsPaging;
			this.resourceDao = createDao(ResourceDao.class, "Resources", tracker, failedCategory, failedPageCategory,
					resourceCount);
			this.orderDao = createDao(OrderDao.class, "Orders", tracker, failedCategory, failedPageCategory, 0);
			this.activityDao = createDao(ActivityDao.class, "Activities", tracker, failedCategory, failedPageCategory,
					0);
		}

		@Override
		public boolean supportsPaging() {
			return this.supportsPaging;
		}

		@Override
		public StrolchTransaction openTx(StrolchRealm realm, Certificate certificate, String action, boolean readOnly) {
			throw new UnsupportedOperationException();
		}

		@Override
		public OrderDao getOrderDao(StrolchTransaction tx) {
			return this.orderDao;
		}

		@Override
		public ResourceDao getResourceDao(StrolchTransaction tx) {
			return this.resourceDao;
		}

		@Override
		public ActivityDao getActivityDao(StrolchTransaction tx) {
			return this.activityDao;
		}

		@Override
		public AuditDao getAuditDao(StrolchTransaction tx) {
			throw new UnsupportedOperationException();
		}

		@Override
		public LogMessageDao getLogMessageDao(StrolchTransaction tx) {
			throw new UnsupportedOperationException();
		}
	}

	private static <T> T createDao(Class<T> daoClass, String category, TransactionTracker tracker,
			String failedCategory, String failedPageCategory, long resourceCount) {
		InvocationHandler handler = (proxy, method, args) -> {
			if (method.getName().equals("queryTypes")) {
				if (tracker.activeElementQueries.get() > 0)
					tracker.categoryBarrierViolations.incrementAndGet();
				tracker.queriedCategories.add(category);
				pauseToExposeConcurrentLoads();
				if (category.equals(failedCategory))
					throw new IllegalStateException("Failed query for " + category);
				if (category.equals("Activities"))
					return Set.of();
				if (category.equals("Resources") && resourceCount > 0)
					if (resourceCount > CachedRealmLoader.MIN_PAGE_SIZE * Runtime.getRuntime().availableProcessors())
						return Set.of("ResourcesLargeType");
				return Set.of(category + "TypeA", category + "TypeB");
			}
			if (method.getName().equals("querySize")) {
				pauseToExposeConcurrentLoads();
				if (args == null || args.length == 0)
					return resourceCount;
				String[] types = (String[]) args[0];
				if (!category.equals("Resources"))
					return 0L;
				if (List.of(types).contains("ResourcesLargeType"))
					return resourceCount;
				return List.of(types).stream().filter(type -> type.equals("ResourcesTypeA")
						|| type.equals("ResourcesTypeB")).count();
			}
			if (method.getName().equals("queryAll")) {
				tracker.elementQueries.incrementAndGet();
				tracker.beginElementQuery();
				try {
					pauseToExposeConcurrentLoads();
					if (args != null && args.length == 3 && category.equals(failedPageCategory))
						throw new IllegalStateException("Failed page query for " + category);
					if (category.equals("Resources") && resourceCount > 0 && args.length == 3) {
						long offset = ((Number) args[1]).longValue();
						long limit = ((Number) args[0]).longValue();
						String[] types = (String[]) args[2];
						if (List.of(types).contains("ResourcesLargeType")) {
							tracker.resourcePageOffsets.add(offset);
							return lazyList(Math.toIntExact(Math.min(limit, resourceCount - offset)));
						}
						return resourcesFor(types);
					}
					if (category.equals("Resources") && resourceCount > 0 && args.length == 1) {
						return resourcesFor((String[]) args[0]);
					}
					return List.of();
				} finally {
					tracker.endElementQuery();
				}
			}
			if (method.getName().equals("toString"))
				return category + "Dao";
			throw new UnsupportedOperationException(method.getName());
		};
		Object dao = Proxy.newProxyInstance(daoClass.getClassLoader(), new Class<?>[]{daoClass}, handler);
		return daoClass.cast(dao);
	}

	private static List<Resource> resourcesFor(String[] types) {
		return List.of(types).stream().map(type -> new Resource(type + "Id", type, type)).toList();
	}

	private static void pauseToExposeConcurrentLoads() throws InterruptedException {
		Thread.sleep(25);
	}

	private static <T> List<T> lazyList(int size) {
		return new AbstractList<>() {
			@Override
			public T get(int index) {
				if (index < 0 || index >= size)
					throw new IndexOutOfBoundsException(index);
				return null;
			}

			@Override
			public int size() {
				return size;
			}
		};
	}

	private static class TrackingResourceMap extends CachedResourceMap {

		private final TransactionTracker tracker;

		private TrackingResourceMap(TestCachedRealm realm, TransactionTracker tracker) {
			super(realm);
			this.tracker = tracker;
		}

		@Override
		void insertAll(List<Resource> elements) {
			if (!elements.isEmpty() && elements.get(0) == null) {
				this.tracker.cachedResources.addAndGet(elements.size());
				return;
			}
			super.insertAll(elements);
			this.tracker.cachedResources.addAndGet(elements.size());
		}
	}

	private static class TransactionTracker {

		private final Semaphore connectionPool;
		private final AtomicInteger openTransactions = new AtomicInteger();
		private final AtomicInteger maxOpenTransactions = new AtomicInteger();
		private final AtomicInteger elementQueries = new AtomicInteger();
		private final AtomicInteger activeElementQueries = new AtomicInteger();
		private final AtomicInteger maxConcurrentElementQueries = new AtomicInteger();
		private final AtomicInteger categoryBarrierViolations = new AtomicInteger();
		private final AtomicLong cachedResources = new AtomicLong();
		private final List<String> queriedCategories = new CopyOnWriteArrayList<>();
		private final List<Long> resourcePageOffsets = new CopyOnWriteArrayList<>();

		private TransactionTracker() {
			this(Integer.MAX_VALUE);
		}

		private TransactionTracker(int connectionLimit) {
			this.connectionPool = new Semaphore(connectionLimit);
		}

		private StrolchTransaction openTransaction() {
			try {
				this.connectionPool.acquire();
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Interrupted while waiting for a connection", e);
			}
			int openCount = this.openTransactions.incrementAndGet();
			this.maxOpenTransactions.accumulateAndGet(openCount, Math::max);
			final StrolchTransaction[] transaction = new StrolchTransaction[1];
			transaction[0] = (StrolchTransaction) Proxy.newProxyInstance(StrolchTransaction.class.getClassLoader(),
					new Class<?>[]{StrolchTransaction.class}, (proxy, method, args) -> {
						if (method.getName().equals("silentThreshold") || method.getName().equals("suppressUpdates"))
							return transaction[0];
						if (method.getName().equals("close")) {
							this.openTransactions.decrementAndGet();
							this.connectionPool.release();
							return null;
						}
						if (method.getName().equals("toString"))
							return "TrackedTransaction";
						throw new UnsupportedOperationException(method.getName());
					});
			return transaction[0];
		}

		private void beginElementQuery() {
			int active = this.activeElementQueries.incrementAndGet();
			this.maxConcurrentElementQueries.accumulateAndGet(active, Math::max);
		}

		private void endElementQuery() {
			this.activeElementQueries.decrementAndGet();
		}
	}
}