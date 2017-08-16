package li.strolch.handler.operationslog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Locator;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class OperationsLog extends StrolchComponent {

	private Map<String, LinkedHashMap<LogMessage, LogMessage>> logMessagesByRealmAndId;
	private Map<String, LinkedHashMap<Locator, LinkedHashSet<LogMessage>>> logMessagesByLocator;
	private int maxMessages;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.maxMessages = configuration.getInt("maxMessages", 10000);

		this.logMessagesByRealmAndId = new HashMap<>();
		this.logMessagesByLocator = new HashMap<>();

		super.initialize(configuration);
	}

	public void addMessage(LogMessage logMessage) {

		// store in global list
		LinkedHashMap<LogMessage, LogMessage> logMessages = this.logMessagesByRealmAndId
				.computeIfAbsent(logMessage.getRealm(), this::newBoundedStringMap);
		logMessages.put(logMessage, logMessage);

		// store under locator
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessagesLocator = this.logMessagesByLocator
				.computeIfAbsent(logMessage.getRealm(), this::newBoundedLocatorMap);
		LinkedHashSet<LogMessage> messages = logMessagesLocator.computeIfAbsent(logMessage.getLocator(),
				(l) -> new LinkedHashSet<LogMessage>());
		messages.add(logMessage);
	}

	public void clearMessages(String realm, Locator locator) {
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages != null)
			logMessages.remove(locator);
	}

	public Optional<Set<LogMessage>> getMessagesFor(String realm, Locator locator) {
		LinkedHashMap<Locator, LinkedHashSet<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages == null)
			return Optional.empty();
		return Optional.ofNullable(logMessages.get(locator));
	}

	public List<LogMessage> getMessages(String realm) {
		LinkedHashMap<LogMessage, LogMessage> logMessages = this.logMessagesByRealmAndId.get(realm);
		if (logMessages == null)
			return Collections.emptyList();

		return new ArrayList<>(logMessages.keySet());
	}

	private LinkedHashMap<LogMessage, LogMessage> newBoundedStringMap(String realm) {
		return new LinkedHashMap<LogMessage, LogMessage>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<LogMessage, LogMessage> eldest) {
				return size() > maxMessages;
			}
		};
	}

	private LinkedHashMap<Locator, LinkedHashSet<LogMessage>> newBoundedLocatorMap(String realm) {
		return new LinkedHashMap<Locator, LinkedHashSet<LogMessage>>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, LinkedHashSet<LogMessage>> eldest) {
				return size() > maxMessages;
			}
		};
	}
}
