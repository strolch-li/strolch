package li.strolch.testbase.runtime;

import static java.util.stream.Collectors.toList;
import static li.strolch.model.Tags.AGENT;
import static li.strolch.runtime.StrolchConstants.SYSTEM_USER_AGENT;
import static org.junit.Assert.assertEquals;

import java.util.List;
import java.util.ResourceBundle;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.LogSeverity;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.Locator;
import li.strolch.persistence.api.LogMessageDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;

public class LogMessagesTestRunner {

	private static final int MAX_MESSAGES = 100;

	private final ComponentContainer container;
	private final String realmName;
	private final OperationsLog operationsLog;
	private final Certificate certificate;

	public LogMessagesTestRunner(ComponentContainer container, String realmName) {
		this.container = container;
		this.realmName = realmName;
		this.operationsLog = this.container.getComponent(OperationsLog.class);
		this.operationsLog.setMaxMessages(MAX_MESSAGES);

		PrivilegeHandler privilegeHandler = this.container.getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runLogMessagesTest() {
		try {
			RuntimeException ex = new RuntimeException();
			ResourceBundle bundle = ResourceBundle.getBundle("li-strolch-testbase");
			LogMessage logMessage = new LogMessage(this.realmName, SYSTEM_USER_AGENT,
					Locator.valueOf(AGENT, "li.strolch.testbase", StrolchAgent.getUniqueId()), LogSeverity.Exception,
					bundle, "test-message").withException(ex).value("reason", ex);
			this.operationsLog.addMessage(logMessage);

			// default is async persisting...
			Thread.sleep(100L);

			StrolchRealm realm = this.container.getRealm(this.realmName);

			if (realm.getMode().isTransient()) {
				List<LogMessage> messages = this.operationsLog.getMessages(realmName);
				assertEquals(2, messages.size());
			} else {
				try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					List<LogMessage> logMessages = logMessageDao.queryLatest(this.realmName, Integer.MAX_VALUE);
					assertEquals(2, logMessages.size());

					LogMessage m = logMessages.get(0);
					assertEquals(logMessage.getId(), m.getId());
					assertEquals(logMessage.getRealm(), m.getRealm());
					assertEquals(logMessage.getLocator(), m.getLocator());
					assertEquals(logMessage.getZonedDateTime(), m.getZonedDateTime());
					assertEquals(logMessage.getSeverity(), m.getSeverity());
					assertEquals(logMessage.getUsername(), m.getUsername());
					assertEquals(logMessage.getStackTrace(), m.getStackTrace());
					assertEquals(logMessage.getKey(), m.getKey());
					assertEquals(logMessage.getMessage(), m.getMessage());
					assertEquals(logMessage.getValues(), m.getValues());
				}
			}

			// initialize with the existing message IDs
			List<String> ids = this.operationsLog.getMessages(this.realmName).stream().map(LogMessage::getId)
					.collect(toList());

			for (int i = 0; i < MAX_MESSAGES * 2; i++) {
				LogMessage m = new LogMessage(this.realmName, SYSTEM_USER_AGENT,
						Locator.valueOf(AGENT, "li.strolch.testbase", StrolchAgent.getUniqueId()),
						LogSeverity.Exception, bundle, "test-message");
				this.operationsLog.addMessage(m);
				ids.add(m.getId());
			}

			// default is async persisting...
			Thread.sleep(1500L);

			int trimSize = (int) (MAX_MESSAGES * 0.1);
			int expectedSize = MAX_MESSAGES - trimSize + 2; // +2 => startup and first message

			if (realm.getMode().isTransient()) {
				List<LogMessage> messages = this.operationsLog.getMessages(this.realmName);
				assertEquals(expectedSize, messages.size());
			} else {

				try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
					LogMessageDao logMessageDao = tx.getPersistenceHandler().getLogMessageDao(tx);
					List<String> logMessageIds = logMessageDao.queryLatest(this.realmName, Integer.MAX_VALUE).stream()
							.map(LogMessage::getId).sorted().collect(toList());
					assertEquals(expectedSize, logMessageIds.size());
					assertEquals(ids.subList(ids.size() - expectedSize, ids.size()), logMessageIds);
				}
			}

		} catch (InterruptedException e) {
			throw new IllegalStateException("Interrupted!");
		}
	}
}
