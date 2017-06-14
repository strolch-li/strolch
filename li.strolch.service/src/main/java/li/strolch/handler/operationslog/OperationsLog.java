package li.strolch.handler.operationslog;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Locator;
import li.strolch.runtime.configuration.ComponentConfiguration;

public class OperationsLog extends StrolchComponent {

	private LinkedHashMap<Locator, LogMessage> logMessages;

	public OperationsLog(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		int maxMessages = configuration.getInt("maxMessages", 10000);

		this.logMessages = new LinkedHashMap<Locator, LogMessage>() {
			private static final long serialVersionUID = 1L;

			@Override
			protected boolean removeEldestEntry(java.util.Map.Entry<Locator, LogMessage> eldest) {
				return size() > maxMessages;
			}
		};

		super.initialize(configuration);
	}

	public void addMessage(LogMessage logMessage) {
		this.logMessages.put(logMessage.getLocator(), logMessage);
	}

	public LogMessage getMessage(Locator locator) {
		return this.logMessages.get(locator);
	}

	public List<LogMessage> getMessages() {
		return this.logMessages.entrySet().stream().map(e -> e.getValue()).collect(Collectors.toList());
	}
}
