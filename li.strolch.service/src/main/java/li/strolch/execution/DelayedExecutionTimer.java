package li.strolch.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Locator;

public interface DelayedExecutionTimer {

	void cancel(Locator locator);

	void execute(String realm, ComponentContainer container, Locator locator, long duration);

	void destroy();
}