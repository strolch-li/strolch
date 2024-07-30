package li.strolch.agent.impl;

import static java.lang.Integer.MAX_VALUE;
import static java.util.concurrent.CompletableFuture.allOf;
import static java.util.concurrent.CompletableFuture.supplyAsync;
import static java.util.concurrent.TimeUnit.SECONDS;

import java.text.MessageFormat;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.function.Supplier;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CachedRealmLoader {

	private static final Logger logger = LoggerFactory.getLogger(CachedRealmLoader.class);
	public static final int MIN_PAGE_SIZE = 200;

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
		logger.info(MessageFormat.format("Loading Model from Database for realm {0}...", realm));

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
		String durationS = StringHelper.formatNanoDuration(duration);
		logger.info(MessageFormat.format("Loading Model from Database for realm {0} took {1}.", realm, durationS));
		logger.info(MessageFormat.format("Loaded {0} Orders", this.nrOfOrders));
		logger.info(MessageFormat.format("Loaded {0} Resources", this.nrOfResources));
		logger.info(MessageFormat.format("Loaded {0} Activities", this.nrOfActivities));
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
			logger.info("Loading {} {} from DB...", nrOfElements, context);

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

		String durationS = StringHelper.formatNanoDuration(System.nanoTime() - start);
		logger.info(MessageFormat.format("Loading of {0} {1} took {2}.", nrOfElements, context, durationS));
	}

	private <T extends StrolchRootElement> void loadElementsPagingAsync(String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicLong counter) {

		long start = System.nanoTime();

		Map<String, Long> sizeByTypes = getSizesByType(daoSupplier);
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		long nrOfElements = sizeByTypes.values().stream().mapToLong(Long::longValue).sum();
		logger.info("Loading {} {} from DB...", nrOfElements, context);

		List<CompletableFuture<List<T>>> tasks = new ArrayList<>();
		sizeByTypes.keySet().stream().sorted(Comparator.comparing(sizeByTypes::get)).forEach(type -> {
			long size = sizeByTypes.get(type);
			if (size < MIN_PAGE_SIZE) {
				logger.info("Loading {} {} of type {} from DB async in parallel...", size, context, type);
				tasks.add(supplyAsync(() -> loadPage(daoSupplier, type, MAX_VALUE, 0)));
			} else {
				long pageSize = Math.max(MIN_PAGE_SIZE, size / Runtime.getRuntime().availableProcessors());
				logger.info("Loading {} {} of type {} in pages of {} from DB async in parallel...", size, context, type,
						pageSize);
				long position = 0;
				while (position < size) {
					long offset = position;
					tasks.add(supplyAsync(() -> loadPage(daoSupplier, type, pageSize, offset)));
					position += pageSize;
				}
			}
		});

		// wait for all tasks to complete
		Throwable failureEx = allOf(tasks.toArray(new CompletableFuture[0])).handle((u, t) -> t).join();
		if (failureEx != null)
			throw new IllegalStateException("Failed to load " + context, failureEx);

		// now insert elements into element map
		tasks.stream().map(CompletableFuture::join).forEach(elements -> {
			elementMap.insertAll(elements);
			counter.addAndGet(elements.size());
		});

		DBC.POST.assertEquals("Expected size should be same as counter", nrOfElements, counter.get());
		String durationS = StringHelper.formatNanoDuration(System.nanoTime() - start);
		logger.info(MessageFormat.format("Loading of {0} {1} took {2}.", counter, context, durationS));
	}

	private <T extends StrolchRootElement> List<T> loadPage(Function<StrolchTransaction, StrolchDao<T>> daoSupplier,
			String type, long pageSize, long offset) {
		try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot", true)
				.silentThreshold(10, SECONDS)
				.suppressUpdates()) {
			return daoSupplier.apply(tx).queryAll(pageSize, offset, type);
		}
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
