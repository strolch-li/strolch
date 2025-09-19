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

		if (this.persistenceHandler.supportsPaging()) {
			loadElementsPagingAsync("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
					this.nrOfResources);
			loadElementsPagingAsync("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap,
					this.nrOfOrders);
			loadElementsPagingAsync("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
					this.nrOfActivities);
		} else {
			loadElements("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
					this.nrOfResources);
			loadElements("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap, this.nrOfOrders);
			loadElements("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
					this.nrOfActivities);
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
		long nrOfElements;
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot_" + context, false)) {
			StrolchDao<T> dao = daoSupplier.apply(tx);
			nrOfElements = dao.querySize();
			logger.info("Loading {} {} synchronously from DB...", nrOfElements, context);

			Set<String> types = dao.queryTypes();
			for (String type : types) {
				long sizeOfType = dao.querySize(type);
				logger.info("Loading {} {} of type {} from DB...", sizeOfType, context, type);

				List<T> elements = dao.queryAll(type);
				elementMap.insertAll(elements);
				counter.addAndGet(elements.size());
			}

			tx.commitOnClose();
		}

		String durationS = formatNanoDuration(System.nanoTime() - start);
		logger.info("Loading of {} {} synchronously took {}.", nrOfElements, context, durationS);
	}

	private <T extends StrolchRootElement> void loadElementsPagingAsync(String context,
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
			logger.info("Loading {} small {} maps from DB...", smallMaps.size(), context);
			long startI = System.nanoTime();
			for (String type : smallMaps.keySet()) {
				counter.addAndGet(loadPage(elementMap, daoSupplier, type, MAX_VALUE, 0));
			}
			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} small {} maps took {}", counter, context, duration);
		}

		logger.info("Loading {} large {} maps from DB in parallel...", sizeByTypes.size(), context);
		long startI = System.nanoTime();
		List<CompletableFuture<Long>> tasks = new ArrayList<>();
		sizeByTypes.keySet().stream().sorted(Comparator.comparing(sizeByTypes::get)).forEach(type -> {
			long size = sizeByTypes.get(type);
			long pageSize = Math.max(MIN_PAGE_SIZE, size / availableProcessors);
			logger.info("Loading {} {} of type {} in {} pages of {} from DB async in parallel...", size, context, type,
					availableProcessors, pageSize);
			long position = 0;
			while (position < size) {
				long offset = position;
				tasks.add(supplyAsync(() -> loadPage(elementMap, daoSupplier, type, pageSize, offset)));
				position += pageSize;
			}
		});

		// wait for all tasks to complete
		Throwable failureEx = allOf(tasks.toArray(new CompletableFuture[0])).handle((_, t) -> t).join();
		if (failureEx != null)
			throw new IllegalStateException("Failed to load " + context, failureEx);
		String duration = formatNanoDuration(System.nanoTime() - startI);
		logger.info("Loading {} large {} maps took {}", counter, context, duration);

		// count all elements inserted into map
		tasks.stream().map(CompletableFuture::join).forEach(counter::addAndGet);

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		duration = formatNanoDuration(System.nanoTime() - start);
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
