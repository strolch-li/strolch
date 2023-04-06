package li.strolch.handler.operationslog;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.ResourceBundle.getBundle;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.model.log.LogMessageState.Information;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

import li.strolch.agent.api.ComponentContainer;
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

	private LinkedBlockingQueue<LogTask> queue;

	private Map<String, Set<LogMessage>> logMessagesByRealmAndId;
	private Map<String, Map<Locator, Set<LogMessage>>> logMessagesByLocator;
	private int maxMessages;
	private ExecutorService executorService;
	private Future<?> handleQueueTask;
	private boolean run;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.maxMessages = configuration.getInt("maxMessages", 10000);

		this.queue = new LinkedBlockingQueue<>();
		this.logMessagesByRealmAndId = new ConcurrentHashMap<>();
		this.logMessagesByLocator = new ConcurrentHashMap<>();

		this.executorService = getSingleThreadExecutor("OperationsLog");

		super.initialize(configuration);
	}

	@Override
	public void start() throws Exception {

		Set<String> realmNames = getContainer().getRealmNames();
		for (String realmName : realmNames) {
			StrolchRealm realm = getContainer().getRealm(realmName);
			if (!realm.getMode().isTransient())
				this.queue.add(() -> loadMessages(realmName));
		}

		this.run = true;
		this.handleQueueTask = this.executorService.submit(this::handleQueue);

		super.start();
	}

	@Override
	public void stop() throws Exception {
		this.run = false;
		if (this.handleQueueTask != null)
			this.handleQueueTask.cancel(true);
		if (this.executorService != null)
			this.executorService.shutdownNow();
		super.stop();
	}

	private void handleQueue() {
		while (this.run) {
			try {
				LogTask poll = this.queue.poll(1, TimeUnit.SECONDS);
				if (poll == null)
					continue;

				poll.run();

			} catch (InterruptedException e) {
				if (!this.run)
					logger.warn("Interrupted!");
				else
					logger.error("Failed to perform a task", e);
			} catch (Exception e) {
				logger.error("Failed to perform a task", e);
			}
		}
	}

	private void loadMessages(String realmName) {
		try {
			runAsAgent(ctx -> {

				logger.info("Loading OperationsLog for realm " + realmName + "...");

				try (StrolchTransaction tx = openTx(realmName, ctx.getCertificate(), true)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					List<LogMessage> messages = logMessageDao.queryLatest(realmName, this.maxMessages);
					logger.info("Loaded " + messages.size() + " messages for OperationsLog for realm " + realmName);
					this.logMessagesByRealmAndId.computeIfAbsent(realmName, OperationsLog::newHashSet).addAll(messages);
				} catch (RuntimeException e) {
					logger.error("Failed to load operations log for realm " + realmName, e);
				}
			});
		} catch (Exception e) {
			logger.error("Failed to load operations logs!", e);
			addMessage(
					new LogMessage(realmName, SYSTEM_USER_AGENT, Locator.valueOf(AGENT, "strolch-agent", getUniqueId()),
							LogSeverity.Exception, Information, getBundle("strolch-agent"),
							"operationsLog.load.failed") //
							.value("reason", e.getMessage()) //
							.withException(e));
		}
	}

	public void setMaxMessages(int maxMessages) {
		this.maxMessages = maxMessages;
	}

	public void addMessage(LogMessage logMessage) {
		if (this.queue != null)
			this.queue.add(() -> _addMessage(logMessage));
	}

	public void removeMessage(LogMessage message) {
		this.queue.add(() -> _removeMessage(message));
	}

	public void removeMessages(Collection<LogMessage> logMessages) {
		this.queue.add(() -> _removeMessages(logMessages));
	}

	public void updateState(String realmName, Locator locator, LogMessageState state) {
		this.queue.add(() -> _updateState(realmName, locator, state));
	}

	public void updateState(String realmName, String id, LogMessageState state) {
		this.queue.add(() -> _updateState(realmName, id, state));
	}

	private void _addMessage(LogMessage logMessage) {
		// store in global list
		String realmName = logMessage.getRealm();
		Set<LogMessage> logMessages = this.logMessagesByRealmAndId.computeIfAbsent(realmName,
				OperationsLog::newHashSet);
		logMessages.add(logMessage);

		// store under locator
		Map<Locator, Set<LogMessage>> logMessagesLocator = this.logMessagesByLocator.computeIfAbsent(realmName,
				this::newBoundedLocatorMap);
		Set<LogMessage> messages = logMessagesLocator.computeIfAbsent(logMessage.getLocator(),
				OperationsLog::newHashSet);
		messages.add(logMessage);

		// prune if necessary
		List<LogMessage> messagesToRemove = _pruneMessages(realmName, logMessages);

		// persist changes for non-transient realms
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.getMode().isTransient())
			persist(realm, logMessage, messagesToRemove);
	}

	private void _removeMessage(LogMessage message) {
		String realmName = message.getRealm();
		Map<Locator, Set<LogMessage>> byLocator = this.logMessagesByLocator.get(realmName);
		if (byLocator != null) {
			Set<LogMessage> messages = byLocator.get(message.getLocator());
			if (messages != null) {
				messages.remove(message);
				if (messages.isEmpty())
					byLocator.remove(message.getLocator());
			}
		}

		Set<LogMessage> messages = this.logMessagesByRealmAndId.get(realmName);
		if (messages != null)
			messages.remove(message);

		// persist changes for non-transient realms
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.getMode().isTransient())
			persist(realm, null, singletonList(message));
	}

	private void _removeMessages(Collection<LogMessage> logMessages) {
		Map<String, List<LogMessage>> messagesByRealm = logMessages.stream()
				.collect(Collectors.groupingBy(LogMessage::getRealm));

		messagesByRealm.forEach((realmName, messages) -> {

			Map<Locator, Set<LogMessage>> byLocator = this.logMessagesByLocator.get(realmName);
			if (byLocator != null) {
				messages.forEach(logMessage -> {
					Set<LogMessage> tmp = byLocator.get(logMessage.getLocator());
					if (tmp != null) {
						tmp.remove(logMessage);
						if (tmp.isEmpty())
							byLocator.remove(logMessage.getLocator());
					}
				});
			}

			Set<LogMessage> byRealm = this.logMessagesByRealmAndId.get(realmName);
			if (byRealm != null)
				messages.removeIf(logMessage -> !byRealm.remove(logMessage));

			// persist changes for non-transient realms
			StrolchRealm realm = getContainer().getRealm(realmName);
			if (!realm.getMode().isTransient())
				persist(realm, null, messages);
		});
	}

	private void _updateState(String realmName, Locator locator, LogMessageState state) {
		getMessagesFor(realmName, locator).ifPresent(logMessages -> {
			logMessages.forEach(logMessage -> logMessage.setState(state));

			StrolchRealm realm = getContainer().getRealm(realmName);
			if (!realm.getMode().isTransient())
				updateStates(realm, logMessages);

		});
	}

	private void _updateState(String realmName, String id, LogMessageState state) {
		Set<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realmName);
		if (logMessages == null)
			return;

		for (LogMessage logMessage : logMessages) {
			if (logMessage.getId().equals(id)) {
				logMessage.setState(state);

				StrolchRealm realm = getContainer().getRealm(realmName);
				if (!realm.getMode().isTransient())
					updateStates(realm, singletonList(logMessage));

			}
		}
	}

	private List<LogMessage> _pruneMessages(String realm, Set<LogMessage> logMessages) {
		if (logMessages.size() < this.maxMessages)
			return emptyList();

		List<LogMessage> messagesToRemove = new ArrayList<>();

		int maxDelete = Math.max(1, (int) (this.maxMessages * 0.1));
		int nrOfExcessMessages = logMessages.size() - this.maxMessages;
		if (nrOfExcessMessages > 0)
			maxDelete += nrOfExcessMessages;

		logger.info("Pruning " + maxDelete + " messages from realm " + realm + "...");
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
			handleFailedPersist(realm, e);
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
			handleFailedPersist(realm, e);
		}
	}

	public void clearMessages(String realm, Locator locator) {
		this.queue.add(() -> {
			Map<Locator, Set<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
			if (logMessages != null)
				logMessages.remove(locator);
		});
	}

	public Optional<Set<LogMessage>> getMessagesFor(String realm, Locator locator) {
		Map<Locator, Set<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages == null)
			return Optional.empty();
		Set<LogMessage> result = logMessages.get(locator);
		if (result == null)
			return Optional.empty();
		return Optional.of(new HashSet<>(result));
	}

	public List<LogMessage> getMessages(String realm) {
		Set<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realm);
		if (logMessages == null)
			return emptyList();

		return new ArrayList<>(logMessages);
	}

	private void handleFailedPersist(StrolchRealm realm, Exception e) {
		logger.error("Failed to persist operations logs!", e);
		addMessage(new LogMessage(realm.getRealm(), SYSTEM_USER_AGENT,
				Locator.valueOf(AGENT, "strolch-agent", getUniqueId()), LogSeverity.Exception, Information,
				getBundle("strolch-agent"), "operationsLog.persist.failed") //
				.value("reason", e.getMessage()) //
				.withException(e));
	}

	private Map<Locator, Set<LogMessage>> newBoundedLocatorMap(String realm) {
		return Collections.synchronizedMap(new LinkedHashMap<>() {
			@Override
			protected boolean removeEldestEntry(Map.Entry<Locator, Set<LogMessage>> eldest) {
				return size() > maxMessages;
			}
		});
	}

	private static Set<LogMessage> newHashSet(Object o) {
		return Collections.synchronizedSet(new HashSet<>());
	}

	private interface LogTask {
		void run();
	}
}
