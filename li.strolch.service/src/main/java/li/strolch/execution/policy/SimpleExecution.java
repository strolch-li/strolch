package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.handler.operationslog.LogMessage;
import li.strolch.handler.operationslog.OperationsLog;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * <p>
 * Simple Execution Policy which sets the state of the action depending on the method called.
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class SimpleExecution extends ExecutionPolicy {

	public SimpleExecution(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void toExecution(Action action) {
		setActionState(action, State.EXECUTION);
	}

	@Override
	public void toWarning(Action action) {
		setActionState(action, State.WARNING);
	}

	@Override
	public void toExecuted(Action action) {
		setActionState(action, State.EXECUTED);
	}

	@Override
	public void toStopped(Action action) {
		getDelayedExecutionTimer().cancel(action.getLocator());
		setActionState(action, State.STOPPED);
	}

	@Override
	public void toError(Action action) {
		getDelayedExecutionTimer().cancel(action.getLocator());
		setActionState(action, State.ERROR);
	}

	protected void addMessage(LogMessage message) {
		if (getContainer().hasComponent(OperationsLog.class)) {
			OperationsLog operationsLog = getContainer().getComponent(OperationsLog.class);
			operationsLog.addMessage(message);
		}
	}

	protected void toError(String realm, LogMessage message) {
		addMessage(message);
		getExecutionHandler().toError(realm, message.getLocator());
	}

	protected void toWarning(String realm, LogMessage message) {
		addMessage(message);
		getExecutionHandler().toWarning(realm, message.getLocator());
	}

	@Override
	public void undo() {
		logger.error("Can not undo execution policy " + getClass());
	}
}
