package li.strolch.handler.operationslog;

import jakarta.mail.internet.AddressException;
import jakarta.mail.internet.InternetAddress;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.mail.MailHandler;
import li.strolch.model.Locator;
import li.strolch.model.log.LogMessage;
import li.strolch.model.log.LogMessageState;
import li.strolch.model.log.LogSeverity;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.iso8601.ISO8601;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.ResourceBundle.getBundle;
import static li.strolch.agent.api.StrolchAgent.getUniqueId;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.model.log.LogMessageState.Information;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;

public class OperationsLog extends StrolchComponent {

	public static final String PARAM_MAX_MESSAGES = "maxMessages";
	public static final String PARAM_SEND_MAILS = "sendMails";
	public static final String PARAM_SEND_MAILS_MIN_SEVERITY = "sendMailsMinSeverity";
	public static final String PARAM_SEND_MAILS_RECIPIENTS = "sendMailsRecipients";
	private LinkedBlockingQueue<LogTask> queue;

	private Map<String, LinkedHashSet<LogMessage>> logMessagesByRealmAndId;
	private Map<String, LinkedHashMap<Locator, LinkedHashSet<LogMessage>>> logMessagesByLocator;
	private int maxMessages;
	private ExecutorService executorService;
	private Future<?> handleQueueTask;
	private boolean run;
	private boolean sendMails;
	private LogSeverity sendMailsMinSeverity;
	private String sendMailsRecipients;

	private Map<String, Long> sentMessageHashes;
	private long lastSentHashesPruning;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.sentMessageHashes = new ConcurrentHashMap<>();
		this.sendMails = configuration.getBoolean(PARAM_SEND_MAILS, false);
		this.sendMailsMinSeverity = LogSeverity.valueOf(
				configuration.getString(PARAM_SEND_MAILS_MIN_SEVERITY, LogSeverity.Exception.name()));
		if (this.sendMails) {
			String sendMailsRecipients = configuration.getString(PARAM_SEND_MAILS_RECIPIENTS, null);
			try {
				InternetAddress.parse(sendMailsRecipients);
			} catch (AddressException e) {
				throw new IllegalArgumentException("Failed to parse email recipients " + sendMailsRecipients, e);
			}
			this.sendMailsRecipients = sendMailsRecipients;
		}

		this.maxMessages = configuration.getInt(PARAM_MAX_MESSAGES, 10000);

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
		if (this.sentMessageHashes != null)
			this.sentMessageHashes.clear();
		super.stop();
	}

	private void handleQueue() {
		while (this.run) {
			try {

				// prune sent hashes
				try {
					pruneSentHashes();
				} catch (Exception e) {
					logger.error("Failed to prune hashes", e);
				}

				// now handle add tasks
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

				logger.info("Loading OperationsLog for realm {}...", realmName);

				try (StrolchTransaction tx = openTx(realmName, ctx.getCertificate(), true)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					List<LogMessage> messages = logMessageDao.queryLatest(realmName, this.maxMessages);
					logger.info("Loaded {} messages for OperationsLog for realm {}", messages.size(), realmName);

					// get collections to which to add
					LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.computeIfAbsent(realmName,
							OperationsLog::newHashSet);
					LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessagesByLocator
							= this.logMessagesByLocator.computeIfAbsent(realmName, this::newBoundedLocatorMap);

					// add the messages
					messages.forEach(logMessage -> {
						logMessages.add(logMessage);
						LinkedHashSet<LogMessage> tmp = logMessagesByLocator.computeIfAbsent(logMessage.getLocator(),
								OperationsLog::newHashSet);
						tmp.add(logMessage);
					});
				} catch (RuntimeException e) {
					logger.error("Failed to load operations log for realm {}", realmName, e);
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
		addMessage(logMessage, false);
	}

	public void addMessage(LogMessage logMessage, boolean mailError) {
		if (this.queue != null)
			this.queue.add(() -> _addMessage(logMessage, mailError));
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

	private void _addMessage(LogMessage logMessage, boolean mailError) {
		// store in global list
		String realmName = logMessage.getRealm();
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.computeIfAbsent(realmName,
				OperationsLog::newHashSet);
		logMessages.add(logMessage);

		// store under locator
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessagesLocator
				= this.logMessagesByLocator.computeIfAbsent(realmName, this::newBoundedLocatorMap);
		LinkedHashSet<LogMessage> messages = logMessagesLocator.computeIfAbsent(logMessage.getLocator(),
				OperationsLog::newHashSet);
		messages.add(logMessage);

		// prune if necessary
		List<LogMessage> messagesToRemove = _pruneMessages(realmName, logMessages);

		// persist changes for non-transient realms
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.getMode().isTransient())
			persist(realm, logMessage, messagesToRemove);

		if (!mailError) {
			try {
				sendMessageAsMail(logMessage);
			} catch (Exception e) {
				logger.error("Failed to send mail for log message {}", logMessage.getLocator(), e);
			}
		}
	}

	private void _removeMessage(LogMessage message) {
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
		if (messages != null)
			messages.remove(message);

		// persist changes for non-transient realms
		StrolchRealm realm = getContainer().getRealm(realmName);
		if (!realm.getMode().isTransient())
			persist(realm, null, singletonList(message));
	}

	private void _removeMessages(Collection<LogMessage> logMessages) {
		Map<String, List<LogMessage>> messagesByRealm = logMessages
				.stream()
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
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realmName);
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

	private List<LogMessage> _pruneMessages(String realm, LinkedHashSet<LogMessage> logMessages) {
		if (logMessages.size() < this.maxMessages)
			return emptyList();

		List<LogMessage> messagesToRemove = new ArrayList<>();

		int maxDelete = Math.max(1, (int) (this.maxMessages * 0.1));
		int nrOfExcessMessages = logMessages.size() - this.maxMessages;
		if (nrOfExcessMessages > 0)
			maxDelete += nrOfExcessMessages;

		logger.info("Pruning {} messages from realm {}...", maxDelete, realm);
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
			LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
			if (logMessages != null)
				logMessages.remove(locator);
		});
	}

	public Optional<Set<LogMessage>> getMessagesFor(String realm, Locator locator) {
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages == null)
			return Optional.empty();
		LinkedHashSet<LogMessage> result = logMessages.get(locator);
		if (result == null)
			return Optional.empty();
		return Optional.of(new HashSet<>(result));
	}

	public List<LogMessage> getMessages(String realm) {
		LinkedHashSet<LogMessage> logMessages = this.logMessagesByRealmAndId.get(realm);
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

	private LinkedHashMap<Locator, LinkedHashSet<LogMessage>> newBoundedLocatorMap(String realm) {
		return new LinkedHashMap<>() {
			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, LinkedHashSet<LogMessage>> eldest) {
				return size() > maxMessages;
			}
		};
	}

	private static LinkedHashSet<LogMessage> newHashSet(Object o) {
		return new LinkedHashSet<>();
	}

	private interface LogTask {
		void run();
	}

	private void pruneSentHashes() {
		if (this.sentMessageHashes.isEmpty())
			return;

		long now = System.currentTimeMillis();
		if (now - this.lastSentHashesPruning < TimeUnit.MINUTES.toMillis(10))
			return;

		Set<String> hashes = new HashSet<>(this.sentMessageHashes.keySet());
		for (String hash : hashes) {
			long sentTime = this.sentMessageHashes.get(hash);
			if (now - sentTime > TimeUnit.MINUTES.toMillis(30)) {
				this.sentMessageHashes.remove(hash);
			}
		}

		this.lastSentHashesPruning = now;
	}

	private void sendMessageAsMail(LogMessage logMessage) {
		if (!this.sendMails || logMessage.getSeverity().compareTo(sendMailsMinSeverity) < 0)
			return;

		if (!hasComponent(MailHandler.class))
			return;
		MailHandler mailHandler = getComponent(MailHandler.class);
		if (!mailHandler.isEncryptionEnabled())
			return;

		String hash = logMessage.buildRelevantHash();
		long now = System.currentTimeMillis();
		if (this.sentMessageHashes.containsKey(hash)) {
			long sentTime = this.sentMessageHashes.get(hash);
			if (now - sentTime < TimeUnit.MINUTES.toMillis(30)) {
				logger.warn(
						"LogMessage {} {} has already been sent less than 30min ago as hash is already known. Ignoring.",
						logMessage.getSeverity(), logMessage.getLocator());
				return;
			}
		}
		this.sentMessageHashes.put(hash, now);

		String appName = getAgent().getApplicationName();
		String env = getAgent().getEnvironment();
		Locator locator = trimAgentLocator(logMessage.getLocator());
		String subject = appName + ":" + env + " - " + logMessage.getSeverity() + " - " + locator;
		String stackTrace = logMessage.getStackTrace();
		String text = """
				Dear team,
								
				the following message was logged:
								
				=====================
				Realm: %s
				Severity: %s
				Locator: %s
				Username: %s
				Timestamp: %s
				Message ID: %s
								
				Message:
				---------------------
				%s
								
				StackTrace:
				---------------------
				%s
				=====================
								
				Kind regards
					your server
				""".formatted(logMessage.getRealm(), logMessage.getSeverity(), logMessage.getLocator(),
				logMessage.getUsername(), ISO8601.toString(logMessage.getZonedDateTime()), logMessage.getId(),
				logMessage.getMessage(Locale.ENGLISH), stackTrace == null ? "(none)" : stackTrace);

		mailHandler.sendEncryptedMailAsync(this.sendMailsRecipients, subject, text);
	}

	private static Locator trimAgentLocator(Locator tmp) {
		if (!tmp.get(0).equals(AGENT))
			return tmp.trim(2);

		// remove package name
		String className = tmp.get(2);
		if (!className.contains("."))
			return tmp.trim(3);

		int startClassName = className.lastIndexOf('.') + 1;
		return tmp.trim(2).append(className.substring(startClassName));
	}
}
