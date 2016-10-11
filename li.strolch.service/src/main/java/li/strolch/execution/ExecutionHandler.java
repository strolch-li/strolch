package li.strolch.execution;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.model.Locator;

public abstract class ExecutionHandler extends StrolchComponent {

	public ExecutionHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}
	
	public abstract DelayedExecutionTimer getDelayedExecutionTimer();

	public abstract void toExecution(String realm, Locator locator);

	public abstract void toExecuted(String realm, Locator locator);

	public abstract void toStopped(String realm, Locator locator);

	public abstract void toWarning(String realm, Locator locator);

	public abstract void toError(String realm, Locator locator);
}
