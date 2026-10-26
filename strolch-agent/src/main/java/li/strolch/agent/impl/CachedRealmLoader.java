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
				loadElementsPaging("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
						this.nrOfResources);
				loadElementsPaging("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap,
						this.nrOfOrders);
				loadElementsPaging("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
						this.nrOfActivities);
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

	private <T extends StrolchRootElement> void loadElementsPaging(String context,
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
			logger.info("Loading {} small {} maps from DB sequentially...", smallMaps.size(), context);
			long startI = System.nanoTime();
			long smallCount = 0;
			for (String type : smallMaps.keySet()) {
				smallCount += loadPage(elementMap, daoSupplier, type, MAX_VALUE, 0);
			}
			counter.addAndGet(smallCount);
			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} small {} maps took {}", smallCount, smallMaps.size(), context, duration);
		}

		if (!sizeByTypes.isEmpty()) {
			logger.info("Loading {} large {} maps from DB sequentially...", sizeByTypes.size(), context);
			long startI = System.nanoTime();
			long largeCount = 0;
			for (String type : sizeByTypes.keySet().stream().sorted(Comparator.comparing(sizeByTypes::get)).toList()) {
				long size = sizeByTypes.get(type);
				long pageSize = Math.clamp(size / (availableProcessors * 2L), MIN_PAGE_SIZE, MAX_PAGE_SIZE);
				long nrOfPages = (size + pageSize - 1) / pageSize;
				logger.info("Loading {} {} of type {} in {} pages of {} from DB sequentially...", size, context,
						type, nrOfPages, pageSize);
				long position = 0;
				while (position < size) {
					largeCount += loadPage(elementMap, daoSupplier, type, pageSize, position);
					position += pageSize;
				}
			}
			counter.addAndGet(largeCount);

			String duration = formatNanoDuration(System.nanoTime() - startI);
			logger.info("Loading {} elements across {} large {} maps took {}", largeCount, sizeByTypes.size(), context,
					duration);
		}

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		String duration = formatNanoDuration(System.nanoTime() - start);
		logger.info("Loading of {} {} sequentially took {}.", counter, context, duration);
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
