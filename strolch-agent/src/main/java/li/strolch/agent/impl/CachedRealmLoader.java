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

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.lang.Integer.MAX_VALUE;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.supplyAsync;
import static java.util.concurrent.TimeUnit.SECONDS;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;

public class CachedRealmLoader {

	private static final Logger logger = LoggerFactory.getLogger(CachedRealmLoader.class);
	public static final long MIN_PAGE_SIZE = 200;
	public static final long MAX_PAGE_SIZE = 1000;

	private final CachedRealm realm;
	private final PersistenceHandler persistenceHandler;
	private final PrivilegeContext privilegeContext;

	private final AtomicLong nrOfOrders;
	private final AtomicLong nrOfResources;
	private final AtomicLong nrOfActivities;

	public CachedRealmLoader(CachedRealm realm, PersistenceHandler persistenceHandler,
			PrivilegeContext privilegeContext) {
		this.realm = realm;
		this.persistenceHandler = persistenceHandler;
		this.privilegeContext = privilegeContext;
		this.nrOfOrders = new AtomicLong();
		this.nrOfResources = new AtomicLong();
		this.nrOfActivities = new AtomicLong();
	}

	public void load(String realm) {
		long start = System.nanoTime();
		logger.info("Loading Model from Database for realm {}...", realm);

		try (ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor()) {
			if (this.persistenceHandler.supportsPaging()) {
				CompletableFuture<Void> resources = CompletableFuture.runAsync(() ->
						loadElementsPagingAsync(executor, "Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
								this.nrOfResources), executor);
				CompletableFuture<Void> orders = CompletableFuture.runAsync(() ->
						loadElementsPagingAsync(executor, "Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap,
								this.nrOfOrders), executor);
				CompletableFuture<Void> activities = CompletableFuture.runAsync(() ->
						loadElementsPagingAsync(executor, "Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
								this.nrOfActivities), executor);
				Throwable failureEx = allOf(resources, orders, activities).handle((_, t) -> t).join();
				if (failureEx != null)
					throw new IllegalStateException("Failed to load realm " + realm, failureEx);
			} else {
				CompletableFuture<Void> resources = CompletableFuture.runAsync(() ->
						loadElements(executor, "Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
								this.nrOfResources), executor);
				CompletableFuture<Void> orders = CompletableFuture.runAsync(() ->
						loadElements(executor, "Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap,
								this.nrOfOrders), executor);
				CompletableFuture<Void> activities = CompletableFuture.runAsync(() ->
						loadElements(executor, "Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
								this.nrOfActivities), executor);
				Throwable failureEx = allOf(resources, orders, activities).handle((_, t) -> t).join();
				if (failureEx != null)
					throw new IllegalStateException("Failed to load realm " + realm, failureEx);
			}
		}

		long duration = System.nanoTime() - start;
		String durationS = formatNanoDuration(duration);
		logger.info("Loading Model from Database for realm {} took {}.", realm, durationS);
		logger.info("Loaded {} Orders", this.nrOfOrders);
		logger.info("Loaded {} Resources", this.nrOfResources);
		logger.info("Loaded {} Activities", this.nrOfActivities);
	}

	private <T extends StrolchRootElement> void loadElements(ExecutorService executor, String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicLong counter) {

		long start = System.nanoTime();
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		Set<String> types;
		long nrOfElements;
		try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot_" + context, true).silentThreshold(10, SECONDS)) {
			StrolchDao<T> dao = daoSupplier.apply(tx);
			nrOfElements = dao.querySize();
			types = dao.queryTypes();
		}

		logger.info("Loading {} {} of {} types from DB in parallel...", nrOfElements, context, types.size());

		List<CompletableFuture<Long>> tasks = new ArrayList<>();
		for (String type : types) {
			tasks.add(supplyAsync(() -> {
				try (StrolchTransaction tx = this.realm
						.openTx(getCert(), "strolch_boot", true)
						.silentThreshold(10, SECONDS)
						.suppressUpdates()) {
					StrolchDao<T> dao = daoSupplier.apply(tx);
					List<T> elements = dao.queryAll(type);
					elementMap.insertAll(elements);
					return (long) elements.size();
				}
			}, executor));
		}

		Throwable failureEx = allOf(tasks.toArray(CompletableFuture[]::new)).handle((_, t) -> t).join();
		if (failureEx != null)
			throw new IllegalStateException("Failed to load " + context, failureEx);

		long loadedCount = tasks.stream().mapToLong(CompletableFuture::join).sum();
		counter.addAndGet(loadedCount);

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		String durationS = formatNanoDuration(System.nanoTime() - start);
		logger.info("Loading of {} {} asynchronously took {}.", counter, context, durationS);
	}

	private <T extends StrolchRootElement> void loadElementsPagingAsync(ExecutorService executor, String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicLong counter) {

		long start = System.nanoTime();
		Map<String, Long> sizeByTypes = getSizesByType(daoSupplier);
		CachedElementMap<T> elementMap = elementMapSupplier.get();
		logger.info("Queried {} types from DB took {}", sizeByTypes.size(),
				formatNanoDuration(System.nanoTime() - start));

		int availableProcessors = Runtime.getRuntime().availableProcessors();
		long nrOfElements = sizeByTypes.values().stream().mapToLong(Long::longValue).sum();
		logger.info("Loading {} {} using paging from DB...", nrOfElements, context);

		Map<String, Long> smallMaps = new HashMap<>();
		Set<String> types = new HashSet<>(sizeByTypes.keySet());
		for (String type : types) {
			long size = sizeByTypes.get(type);
			if (size < MIN_PAGE_SIZE * availableProcessors) {
				smallMaps.put(type, size);
				sizeByTypes.remove(type);
			}
		}

		if (!smallMaps.isEmpty()) {
			logger.info("Loading {} small {} maps from DB in parallel...", smallMaps.size(), context);
			long startI = System.nanoTime();
			List<CompletableFuture<Long>> smallTasks = new ArrayList<>();
			for (String type : smallMaps.keySet()) {
				smallTasks.add(supplyAsync(() -> loadPage(elementMap, daoSupplier, type, MAX_VALUE, 0), executor));
			}
			Throwable failureEx = allOf(smallTasks.toArray(CompletableFuture[]::new)).handle((_, t) -> t).join();
			if (failureEx != null)
				throw new IllegalStateException("Failed to load small " + context + " maps", failureEx);
			long smallCount = smallTasks.stream().mapToLong(CompletableFuture::join).sum();
			counter.addAndGet(smallCount);
			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} small {} maps took {}", smallCount, smallMaps.size(), context, duration);
		}

		if (!sizeByTypes.isEmpty()) {
			logger.info("Loading {} large {} maps from DB in parallel...", sizeByTypes.size(), context);
			long startI = System.nanoTime();
			List<CompletableFuture<Long>> tasks = new ArrayList<>();
			sizeByTypes.keySet().stream().sorted(Comparator.comparing(sizeByTypes::get)).forEach(type -> {
				long size = sizeByTypes.get(type);
				long pageSize = Math.clamp(size / (availableProcessors * 2L), MIN_PAGE_SIZE, MAX_PAGE_SIZE);
				long nrOfPages = (size + pageSize - 1) / pageSize;
				logger.info("Loading {} {} of type {} in {} pages of {} from DB async in parallel...", size, context,
						type, nrOfPages, pageSize);
				long position = 0;
				while (position < size) {
					long offset = position;
					tasks.add(supplyAsync(() -> loadPage(elementMap, daoSupplier, type, pageSize, offset), executor));
					position += pageSize;
				}
			});

			// wait for all tasks to complete
			Throwable failureEx = allOf(tasks.toArray(CompletableFuture[]::new)).handle((_, t) -> t).join();
			if (failureEx != null)
				throw new IllegalStateException("Failed to load " + context, failureEx);

			// count all elements inserted into map
			long largeCount = tasks.stream().mapToLong(CompletableFuture::join).sum();
			counter.addAndGet(largeCount);

			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} large {} maps took {}", largeCount, sizeByTypes.size(), context,
					duration);
		}

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		String duration = formatNanoDuration(System.nanoTime() - start);
		logger.info("Loading of {} {} asynchronously took {}.", counter, context, duration);
	}

	private <T extends StrolchRootElement> long loadPage(CachedElementMap<T> elementMap,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, String type, long pageSize, long offset) {
		long count;
		try (StrolchTransaction tx = this.realm
				.openTx(getCert(), "strolch_boot", true)
				.silentThreshold(10, SECONDS)
				.suppressUpdates()) {
			List<T> elements = daoSupplier.apply(tx).queryAll(pageSize, offset, type);
			elementMap.insertAll(elements);
			count = elements.size();
		}

		return count;
	}

	private <T extends StrolchRootElement> Map<String, Long> getSizesByType(
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier) {
		Map<String, Long> sizeByTypes = new HashMap<>();
		try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot", true).silentThreshold(10, SECONDS)) {
			StrolchDao<T> dao = daoSupplier.apply(tx);
			Set<String> types = dao.queryTypes();
			for (String type : types) {
				sizeByTypes.put(type, dao.querySize(type));
			}
		}
		return sizeByTypes;
	}

	private Certificate getCert() {
		return this.privilegeContext.getCertificate();
	}
}
