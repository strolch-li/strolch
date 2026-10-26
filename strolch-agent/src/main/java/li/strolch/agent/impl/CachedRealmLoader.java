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
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.lang.Integer.MAX_VALUE;
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

		try {
			if (this.persistenceHandler.supportsPaging()) {
				int parallelism = Math.max(1, Runtime.getRuntime().availableProcessors());
				try (ExecutorService executor = Executors.newFixedThreadPool(parallelism)) {
					loadElementsPaging(executor, parallelism, "Resources", this.persistenceHandler::getResourceDao,
							this.realm::getResourceMap, this.nrOfResources);
					loadElementsPaging(executor, parallelism, "Orders", this.persistenceHandler::getOrderDao,
							this.realm::getOrderMap, this.nrOfOrders);
					loadElementsPaging(executor, parallelism, "Activities", this.persistenceHandler::getActivityDao,
							this.realm::getActivityMap, this.nrOfActivities);
				}
			} else {
				loadElements("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
						this.nrOfResources);
				loadElements("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap, this.nrOfOrders);
				loadElements("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
						this.nrOfActivities);
			}
		} catch (RuntimeException e) {
			throw new IllegalStateException("Failed to load realm " + realm, e);
		}

		long duration = System.nanoTime() - start;
		String durationS = formatNanoDuration(duration);
		logger.info("Loading Model from Database for realm {} took {}.", realm, durationS);
		logger.info("Loaded {} Orders", this.nrOfOrders);
		logger.info("Loaded {} Resources", this.nrOfResources);
		logger.info("Loaded {} Activities", this.nrOfActivities);
	}

	private <T extends StrolchRootElement> void loadElements(String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicLong counter) {

		long start = System.nanoTime();
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		try {
			Set<String> types;
			long nrOfElements;
			try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot_" + context, true)
					.silentThreshold(10, SECONDS)) {
				StrolchDao<T> dao = daoSupplier.apply(tx);
				nrOfElements = dao.querySize();
				types = dao.queryTypes();
			}

			logger.info("Loading {} {} of {} types from DB sequentially...", nrOfElements, context, types.size());

			long loadedCount = 0;
			for (String type : types) {
				try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot", true)
						.silentThreshold(10, SECONDS).suppressUpdates()) {
					List<T> elements = daoSupplier.apply(tx).queryAll(type);
					elementMap.insertAll(elements);
					loadedCount += elements.size();
				}
			}
			counter.addAndGet(loadedCount);

			DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
			String durationS = formatNanoDuration(System.nanoTime() - start);
			logger.info("Loading of {} {} sequentially took {}.", counter, context, durationS);
		} catch (RuntimeException e) {
			throw new IllegalStateException("Failed to load " + context, e);
		}
	}

	private <T extends StrolchRootElement> void loadElementsPaging(ExecutorService executor, int parallelism, String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicLong counter) {

		long start = System.nanoTime();
		Map<String, Long> sizeByTypes = getSizesByType(daoSupplier);
		CachedElementMap<T> elementMap = elementMapSupplier.get();
		logger.info("Queried {} types from DB took {}", sizeByTypes.size(),
				formatNanoDuration(System.nanoTime() - start));

		long nrOfElements = sizeByTypes.values().stream().mapToLong(Long::longValue).sum();
		logger.info("Loading {} {} using paging from DB...", nrOfElements, context);

		Map<String, Long> smallMaps = new HashMap<>();
		Set<String> types = new HashSet<>(sizeByTypes.keySet());
		for (String type : types) {
			long size = sizeByTypes.get(type);
			if (size < MIN_PAGE_SIZE * parallelism) {
				smallMaps.put(type, size);
				sizeByTypes.remove(type);
			}
		}

		if (!smallMaps.isEmpty()) {
			logger.info("Loading {} small {} maps from DB in parallel...", smallMaps.size(), context);
			long startI = System.nanoTime();
			Iterator<Callable<Long>> smallTasks = smallMaps.keySet().stream()
					.<Callable<Long>>map(type -> () -> loadPage(elementMap, daoSupplier, type, MAX_VALUE, 0)).iterator();
			long smallCount = loadTasks(executor, parallelism, smallTasks);
			counter.addAndGet(smallCount);
			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} small {} maps took {}", smallCount, smallMaps.size(), context, duration);
		}

		if (!sizeByTypes.isEmpty()) {
			logger.info("Loading {} large {} maps from DB in parallel...", sizeByTypes.size(), context);
			long startI = System.nanoTime();
			Iterator<String> typeIterator = sizeByTypes.keySet().stream()
					.sorted(Comparator.comparing(sizeByTypes::get)).iterator();
			Iterator<Callable<Long>> pageTasks = new Iterator<>() {
				private String type;
				private long size;
				private long pageSize;
				private long offset;

				@Override
				public boolean hasNext() {
					while (this.type == null || this.offset >= this.size) {
						if (!typeIterator.hasNext())
							return false;
						this.type = typeIterator.next();
						this.size = sizeByTypes.get(this.type);
						this.pageSize = Math.clamp(this.size / (parallelism * 2L), MIN_PAGE_SIZE, MAX_PAGE_SIZE);
						this.offset = 0;
						long nrOfPages = (this.size + this.pageSize - 1) / this.pageSize;
						logger.info("Loading {} {} of type {} in {} pages of {} from DB in parallel...", this.size,
								context, this.type, nrOfPages, this.pageSize);
					}
					return true;
				}

				@Override
				public Callable<Long> next() {
					if (!hasNext())
						throw new NoSuchElementException();
					String taskType = this.type;
					long taskOffset = this.offset;
					long taskPageSize = this.pageSize;
					this.offset += this.pageSize;
					return () -> loadPage(elementMap, daoSupplier, taskType, taskPageSize, taskOffset);
				}
			};
			long largeCount = loadTasks(executor, parallelism, pageTasks);
			counter.addAndGet(largeCount);

			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} large {} maps took {}", largeCount, sizeByTypes.size(), context,
					duration);
		}

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		String duration = formatNanoDuration(System.nanoTime() - start);
		logger.info("Loading of {} {} in parallel took {}.", counter, context, duration);
	}

	private long loadTasks(ExecutorService executor, int parallelism, Iterator<Callable<Long>> tasks) {
		CompletionService<Long> completionService = new ExecutorCompletionService<>(executor);
		int inFlight = 0;
		long loadedCount = 0;
		try {
			while (tasks.hasNext() || inFlight > 0) {
				while (tasks.hasNext() && inFlight < parallelism) {
					completionService.submit(tasks.next());
					inFlight++;
				}
				if (inFlight > 0) {
					loadedCount += completionService.take().get();
					inFlight--;
				}
			}
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted while loading cached elements", e);
		} catch (ExecutionException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException runtimeException)
				throw runtimeException;
			throw new IllegalStateException("Failed to load cached elements", cause);
		}
		return loadedCount;
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
