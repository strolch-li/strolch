package li.strolch.handler.operationslog;

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

	private Map<String, LinkedHashMap<Locator, LogMessage>> logMessagesByRealm;
	private int maxMessages;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		this.maxMessages = configuration.getInt("maxMessages", 10000);

		this.logMessagesByRealm = new HashMap<>();

		super.initialize(configuration);
	}

	public void addMessage(LogMessage logMessage) {

		LinkedHashMap<Locator, LogMessage> logMessages = this.logMessagesByRealm.computeIfAbsent(logMessage.getRealm(),
				this::newBoundedMap);

		logMessages.put(logMessage.getLocator(), logMessage);
	}

	private LinkedHashMap<Locator, LogMessage> newBoundedMap(String realm) {
		return new LinkedHashMap<Locator, LogMessage>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, LogMessage> eldest) {
				return size() > maxMessages;
			}
		};
	}

	public Optional<LogMessage> getMessage(String realm, Locator locator) {
		LinkedHashMap<Locator, LogMessage> logMessages = this.logMessagesByRealm.computeIfAbsent(realm,
				this::newBoundedMap);
		return Optional.ofNullable(logMessages.get(locator));
	}

	public List<LogMessage> getMessages(String realm) {
		LinkedHashMap<Locator, LogMessage> logMessages = this.logMessagesByRealm.get(realm);
		if (logMessages == null)
			return Collections.emptyList();

		return logMessages.entrySet().stream().map(e -> e.getValue()).collect(Collectors.toList());
	}
}
