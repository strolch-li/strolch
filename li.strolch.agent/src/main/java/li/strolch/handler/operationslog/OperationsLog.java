package li.strolch.handler.operationslog;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class OperationsLog extends StrolchComponent {

	private Map<String, LinkedHashSet<LogMessage>> logMessagesByRealmAndId;
	private Map<String, LinkedHashMap<Locator, LinkedHashSet<LogMessage>>> logMessagesByLocator;
	private int maxMessages;
	private ExecutorService executorService;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.maxMessages = configuration.getInt("maxMessages", 10000);

		this.logMessagesByRealmAndId = new HashMap<>();
		this.logMessagesByLocator = new HashMap<>();

		this.executorService = getSingleThreadExecutor("OperationsLog");

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {
		runAsAgent(ctx -> {
			Set<String> realmNames = getContainer().getRealmNames();
			for (String realmName : realmNames) {

				// ignore for transient realms
				if (getContainer().getRealm(realmName).getMode().isTransient())
					continue;

				logger.info("Loading OperationsLog for realm " + realmName + "...");

				try (StrolchTransaction tx = openTx(realmName, ctx.getCertificate(), true)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					List<LogMessage> messages = logMessageDao.queryLatest(realmName, this.maxMessages);
					logger.info("Loaded " + messages.size() + " messages for OperationsLog for realm " + realmName);
					this.logMessagesByRealmAndId.put(realmName, new LinkedHashSet<>(messages));
				} catch (RuntimeException e) {
					logger.error("Failed to load operations log for realm " + realmName, e);
				}
			}
		});

		super.start();
	}

	public void setMaxMessages(int maxMessages) {
		this.maxMessages = maxMessages;
	}

	public synchronized void addMessage(LogMessage logMessage) {
		if (this.logMessagesByRealmAndId == null)
			return;

		// store in global list
		String realmName = logMessage.getRealm();
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.computeIfAbsent(realmName,
				r -> new LinkedHashSet<>());
		logMessages.add(logMessage);

		// store under locator
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessagesLocator = this.logMessagesByLocator.computeIfAbsent(
				realmName, this::newBoundedLocatorMap);
		LinkedHashSet<LogMessage> messages = logMessagesLocator.computeIfAbsent(logMessage.getLocator(),
				(l) -> new LinkedHashSet<>());
		messages.add(logMessage);

		// prune if necessary
		List<LogMessage> messagesToRemove = pruneMessages(logMessages);

		// persist changes for non-transient realms
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.getMode().isTransient())
			this.executorService.submit(() -> persist(realm, logMessage, messagesToRemove));
	}

	public void removeMessage(LogMessage message) {

		String realmName = message.getRealm();
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> byLocator = this.logMessagesByLocator.get(realmName);
		if (byLocator != null) {
			LinkedHashSet<LogMessage> messages = byLocator.get(message.getLocator());
			if (messages != null) {
				messages.remove(message);
				if (messages.isEmpty())
					byLocator.remove(message.getLocator());
			}
		}

		LinkedHashSet<LogMessage> messages = this.logMessagesByRealmAndId.get(realmName);
		if (messages != null) {
			messages.remove(message);

			// persist changes for non-transient realms
			StrolchRealm realm = getContainer().getRealm(realmName);
			if (!realm.getMode().isTransient())
				this.executorService.submit(() -> persist(realm, null, singletonList(message)));
		}
	}

	public synchronized void removeMessages(Collection<LogMessage> logMessages) {

		Map<String, List<LogMessage>> messagesByRealm = logMessages.stream()
				.collect(Collectors.groupingBy(LogMessage::getRealm));

		messagesByRealm.forEach((realmName, messages) -> {

			LinkedHashMap<Locator, LinkedHashSet<LogMessage>> byLocator = this.logMessagesByLocator.get(realmName);
			if (byLocator != null) {
				messages.forEach(logMessage -> {
					LinkedHashSet<LogMessage> tmp = byLocator.get(logMessage.getLocator());
					if (tmp != null) {
						tmp.remove(logMessage);
						if (tmp.isEmpty())
							byLocator.remove(logMessage.getLocator());
					}
				});
			}

			LinkedHashSet<LogMessage> byRealm = this.logMessagesByRealmAndId.get(realmName);
			if (byRealm != null) {
				messages.removeIf(logMessage -> !byRealm.remove(logMessage));

				// persist changes for non-transient realms
				StrolchRealm realm = getContainer().getRealm(realmName);
				if (!realm.getMode().isTransient())
					this.executorService.submit(() -> persist(realm, null, messages));
			}
		});
	}

	public synchronized void updateState(String realmName, Locator locator, LogMessageState state) {
		getMessagesFor(realmName, locator).ifPresent(logMessages -> {
			logMessages.forEach(logMessage -> logMessage.setState(state));

			StrolchRealm realm = getContainer().getRealm(realmName);
			if (!realm.getMode().isTransient()) {
				this.executorService.submit(() -> updateStates(realm, logMessages));
			}
		});
	}

	public synchronized void updateState(String realmName, String id, LogMessageState state) {
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realmName);
		for (LogMessage logMessage : logMessages) {
			if (logMessage.getId().equals(id)) {
				logMessage.setState(state);

				StrolchRealm realm = getContainer().getRealm(realmName);
				if (!realm.getMode().isTransient()) {
					this.executorService.submit(() -> updateStates(realm, singletonList(logMessage)));
				}
			}
		}
	}

	private List<LogMessage> pruneMessages(LinkedHashSet<LogMessage> logMessages) {
		if (logMessages.size() < this.maxMessages)
			return emptyList();

		List<LogMessage> messagesToRemove = new ArrayList<>();

		int maxDelete = Math.max(1, (int) (this.maxMessages * 0.1));
		int nrOfExcessMsgs = logMessages.size() - this.maxMessages;
		if (nrOfExcessMsgs > 0)
			maxDelete += nrOfExcessMsgs;

		logger.info("Pruning " + maxDelete + " messages from OperationsLog...");
		Iterator<LogMessage> iterator = logMessages.iterator();
		while (maxDelete > 0 && iterator.hasNext()) {
			LogMessage messageToRemove = iterator.next();
			messagesToRemove.add(messageToRemove);
			iterator.remove();
			maxDelete--;
		}

		return messagesToRemove;
	}

	private void persist(StrolchRealm realm, LogMessage logMessage, List<LogMessage> messagesToRemove) {
		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = realm.openTx(ctx.getCertificate(), getClass(), false)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					if (messagesToRemove != null && !messagesToRemove.isEmpty())
						logMessageDao.removeAll(messagesToRemove);
					if (logMessage != null)
						logMessageDao.save(logMessage);
					tx.commitOnClose();
				}
			});
		} catch (Exception e) {
			logger.error("Failed to persist operations logs!", e);
			synchronized (this) {
				this.logMessagesByRealmAndId.computeIfAbsent(realm.getRealm(), r -> new LinkedHashSet<>())
						.add(new LogMessage(realm.getRealm(), SYSTEM_USER_AGENT,
								Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()), LogSeverity.Info,
								LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
								"operationsLog.persist.failed") //
								.value("reason", e.getMessage()) //
								.withException(e));
			}
		}
	}

	private void updateStates(StrolchRealm realm, Collection<LogMessage> logMessages) {
		try {
			runAsAgent(ctx -> {
				try (StrolchTransaction tx = realm.openTx(ctx.getCertificate(), getClass(), false)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					logMessageDao.updateStates(logMessages);
					tx.commitOnClose();
				}
			});
		} catch (Exception e) {
			logger.error("Failed to persist operations logs!", e);
			synchronized (this) {
				this.logMessagesByRealmAndId.computeIfAbsent(realm.getRealm(), r -> new LinkedHashSet<>())
						.add(new LogMessage(realm.getRealm(), SYSTEM_USER_AGENT,
								Locator.valueOf(AGENT, "strolch-agent", StrolchAgent.getUniqueId()), LogSeverity.Info,
								LogMessageState.Information, ResourceBundle.getBundle("strolch-agent"),
								"operationsLog.persist.failed") //
								.value("reason", e.getMessage()) //
								.withException(e));
			}
		}
	}

	public synchronized void clearMessages(String realm, Locator locator) {
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages != null)
			logMessages.remove(locator);
	}

	public synchronized Optional<Set<LogMessage>> getMessagesFor(String realm, Locator locator) {
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages == null)
			return Optional.empty();
		LinkedHashSet<LogMessage> result = logMessages.get(locator);
		if (result == null)
			return Optional.empty();
		return Optional.of(new HashSet<>(result));
	}

	public synchronized List<LogMessage> getMessages(String realm) {
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realm);
		if (logMessages == null)
			return emptyList();

		return new ArrayList<>(logMessages);
	}

	private LinkedHashMap<Locator, LinkedHashSet<LogMessage>> newBoundedLocatorMap(String realm) {
		return new LinkedHashMap<>() {
			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, LinkedHashSet<LogMessage>> eldest) {
				return size() > maxMessages;
			}
		};
	}
}
