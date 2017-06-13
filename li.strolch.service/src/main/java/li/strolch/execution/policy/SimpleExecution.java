package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
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

	protected void toStarting(Action action) {
		setActionState(action, State.STARTING);
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

	@Override
	public void undo() {
		logger.error("Can not undo execution policy " + getClass());
	}
}
