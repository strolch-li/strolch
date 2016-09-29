package li.strolch.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Locator;

public abstract class ExecutionHandler extends StrolchComponent {

	public ExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	public abstract void execute(Locator locator);

	public abstract void executed(Locator locator);

	public abstract void stop(Locator locator);
}
