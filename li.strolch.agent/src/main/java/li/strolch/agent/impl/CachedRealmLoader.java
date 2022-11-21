package li.strolch.agent.impl;

import static java.util.concurrent.TimeUnit.SECONDS;

import java.text.MessageFormat;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Supplier;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.PersistenceHandler;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CachedRealmLoader {

	private static final Logger logger = LoggerFactory.getLogger(CachedRealmLoader.class);

	private final CachedRealm realm;
	private final PersistenceHandler persistenceHandler;
	private final PrivilegeContext privilegeContext;

	private final AtomicInteger nrOfOrders;
	private final AtomicInteger nrOfResources;
	private final AtomicInteger nrOfActivities;

	public CachedRealmLoader(CachedRealm realm, PersistenceHandler persistenceHandler,
			PrivilegeContext privilegeContext) {
		this.realm = realm;
		this.persistenceHandler = persistenceHandler;
		this.privilegeContext = privilegeContext;
		this.nrOfOrders = new AtomicInteger();
		this.nrOfResources = new AtomicInteger();
		this.nrOfActivities = new AtomicInteger();
	}

	public void load(String realm) {
		long start = System.nanoTime();
		logger.info(MessageFormat.format("Loading Model from Database for realm {0}...", realm));

		if (this.persistenceHandler.supportsPaging()) {
			loadPagingAsync();
		} else {
			load("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap, this.nrOfResources);
			load("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap, this.nrOfOrders);
			load("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
					this.nrOfActivities);
		}

		long duration = System.nanoTime() - start;
		String durationS = StringHelper.formatNanoDuration(duration);
		logger.info(MessageFormat.format("Loading Model from Database for realm {0} took {1}.", realm, durationS));
		logger.info(MessageFormat.format("Loaded {0} Orders", this.nrOfOrders));
		logger.info(MessageFormat.format("Loaded {0} Resources", this.nrOfResources));
		logger.info(MessageFormat.format("Loaded {0} Activities", this.nrOfActivities));
	}

	private <T extends StrolchRootElement> void load(String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicInteger counter) {

		long start = System.nanoTime();
		long nrOfElements;
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot_" + context, false)) {
			StrolchDao<T> dao = daoSupplier.apply(tx);
			nrOfElements = dao.querySize();
			logger.info("Loading " + nrOfElements + " " + context + " from DB...");

			Set<String> types = dao.queryTypes();
			for (String type : types) {
				long sizeOfType = dao.querySize(type);
				logger.info("Loading " + sizeOfType + " " + context + " of type " + type + " from DB...");

				List<T> elements = dao.queryAll(type);
				elements.forEach(elementMap::insert);
				counter.addAndGet(elements.size());
			}

			tx.commitOnClose();
		}

		String durationS = StringHelper.formatNanoDuration(System.nanoTime() - start);
		logger.info(MessageFormat.format("Loading of {0} {1} took {2}.", nrOfElements, context, durationS));
	}

	private void loadPagingAsync() {
		loadElementsPagingAsync("Resources", this.persistenceHandler::getResourceDao, this.realm::getResourceMap,
				this.nrOfResources);
		loadElementsPagingAsync("Orders", this.persistenceHandler::getOrderDao, this.realm::getOrderMap,
				this.nrOfOrders);
		loadElementsPagingAsync("Activities", this.persistenceHandler::getActivityDao, this.realm::getActivityMap,
				this.nrOfActivities);
	}

	private <T extends StrolchRootElement> void loadElementsPagingAsync(String context,
			Function<StrolchTransaction, StrolchDao<T>> daoSupplier, Supplier<CachedElementMap<T>> elementMapSupplier,
			AtomicInteger counter) {

		long start = System.nanoTime();

		Map<String, Long> sizeByTypes = getSizesByType(daoSupplier);
		CachedElementMap<T> elementMap = elementMapSupplier.get();

		long nrOfElements = sizeByTypes.values().stream().mapToLong(Long::longValue).sum();
		logger.info("Loading " + nrOfElements + " " + context + " from DB...");

		List<CompletableFuture<Void>> tasks = new ArrayList<>();
		sizeByTypes.forEach((type, size) -> {

			long pageSize = Math.max(1, size / Runtime.getRuntime().availableProcessors());

			logger.info("Loading " + size + " " + context + " of type " + type + " from DB async in parallel...");
			long position = 0;
			while (position < size) {
				long p = position;

				tasks.add(CompletableFuture.runAsync(() -> {
					try (StrolchTransaction tx = this.realm.openTx(getCert(), "strolch_boot", true)
							.silentThreshold(10, SECONDS)
							.suppressUpdates()) {
						List<T> elements = daoSupplier.apply(tx).queryAll(pageSize, p, type);
						elements.forEach(elementMap::insert);
						counter.addAndGet(elements.size());
					}
				}));

				position += pageSize;
			}
		});

		Throwable failureEx;
		try {
			failureEx = CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0]))
					.handle((unused, throwable) -> throwable)
					.get();
		} catch (InterruptedException | ExecutionException e) {
			throw new IllegalStateException("Failed to load " + context, e);
		}

		if (failureEx != null)
			throw new IllegalStateException("Failed to load " + context, failureEx);

		String durationS = StringHelper.formatNanoDuration(System.nanoTime() - start);
		logger.info(MessageFormat.format("Loading of {0} {1} took {2}.", nrOfElements, context, durationS));
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
