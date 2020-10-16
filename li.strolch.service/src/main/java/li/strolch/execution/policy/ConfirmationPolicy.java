package li.strolch.execution.policy;

import li.strolch.model.activity.Action;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

/**
 * The {@link ConfirmationPolicy} is called for every state change of an {@link Action}. This is where extra tasks can
 * be performed when an {@link Action} enters a specific state, e.g. send an e-mail, etc.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ConfirmationPolicy extends StrolchPolicy {

	public static PolicyDef DEFAULT_CONFIRMATION = PolicyDef
			.valueOf(ConfirmationPolicy.class.getSimpleName(), "key:DefaultConfirmation");

	public ConfirmationPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public void toCreated(Action action) {
		// do nothing
	}

	public void toPlanning(Action action) {
		// do nothing
	}

	public void toPlanned(Action action) {
		// do nothing
	}

	public void toExecutable(Action action) {
		// do nothing
	}

	public void toExecution(Action action) {
		// do nothing
	}

	public void toStopped(Action action) {
		// do nothing
	}

	public void toWarning(Action action) {
		// do nothing
	}

	public void toError(Action action) {
		// do nothing
	}

	public void toExecuted(Action action) {
		// do nothing
	}

	public void toClosed(Action action) {
		// do nothing
	}

	@Override
	public void undo() {
		// nothing to undo
	}

	/**
	 * Calls the appropriate confirmation method depending on the state of the {@link Action}
	 *
	 * @param action
	 * 		the action for which to perform the confirmation call
	 */
	public void doConfirmation(Action action) {
		switch (action.getState()) {

		case CREATED:
			toCreated(action);
			break;
		case PLANNING:
			toPlanning(action);
			break;
		case PLANNED:
			toPlanned(action);
			break;
		case EXECUTABLE:
			toExecutable(action);
			break;
		case EXECUTION:
			toExecution(action);
			break;
		case WARNING:
			toWarning(action);
			break;
		case ERROR:
			toError(action);
			break;
		case STOPPED:
			toStopped(action);
			break;
		case EXECUTED:
			toExecuted(action);
			break;
		case CLOSED:
			toClosed(action);
			break;
		}
	}
}
