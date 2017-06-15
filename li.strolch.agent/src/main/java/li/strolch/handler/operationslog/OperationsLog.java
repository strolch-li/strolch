package li.strolch.handler.operationslog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Locator;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class OperationsLog extends StrolchComponent {

	private Map<String, LinkedHashMap<String, LogMessage>> logMessagesById;
	private Map<String, LinkedHashMap<Locator, List<LogMessage>>> logMessagesByLocator;
	private int maxMessages;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.maxMessages = configuration.getInt("maxMessages", 10000);

		this.logMessagesById = new HashMap<>();
		this.logMessagesByLocator = new HashMap<>();

		super.initialize(configuration);
	}

	public void addMessage(LogMessage logMessage) {

		// store in global list
		LinkedHashMap<String, LogMessage> logMessages = this.logMessagesById.computeIfAbsent(logMessage.getRealm(),
				this::newBoundedStringMap);
		logMessages.put(logMessage.getId(), logMessage);

		// store under locator
		LinkedHashMap<Locator, List<LogMessage>> logMessagesLocator = this.logMessagesByLocator
				.computeIfAbsent(logMessage.getRealm(), this::newBoundedLocatorMap);
		List<LogMessage> messages = logMessagesLocator.computeIfAbsent(logMessage.getLocator(),
				(l) -> new ArrayList<LogMessage>());
		messages.add(logMessage);
	}

	public void clearMessages(String realm, Locator locator) {
		LinkedHashMap<Locator, List<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages != null)
			logMessages.remove(locator);
	}

	public Optional<List<LogMessage>> getMessagesFor(String realm, Locator locator) {
		LinkedHashMap<Locator, List<LogMessage>> logMessages = this.logMessagesByLocator.get(realm);
		if (logMessages == null)
			return Optional.empty();
		return Optional.ofNullable(logMessages.get(locator));
	}

	public List<LogMessage> getMessages(String realm) {
		LinkedHashMap<String, LogMessage> logMessages = this.logMessagesById.get(realm);
		if (logMessages == null)
			return Collections.emptyList();

		return logMessages.entrySet().stream().map(e -> e.getValue()).collect(Collectors.toList());
	}

	private LinkedHashMap<String, LogMessage> newBoundedStringMap(String realm) {
		return new LinkedHashMap<String, LogMessage>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<String, LogMessage> eldest) {
				return size() > maxMessages;
			}
		};
	}

	private LinkedHashMap<Locator, List<LogMessage>> newBoundedLocatorMap(String realm) {
		return new LinkedHashMap<Locator, List<LogMessage>>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, List<LogMessage>> eldest) {
				return size() > maxMessages;
			}
		};
	}
}
