package li.strolch.execution.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

/**
 * The {@link ConfirmationPolicy} is called for every state change of an {@link Action}. This is where extra tasks can
 * be performed when an {@link Action} enters a specific state, e.g. send an e-mail, etc.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ConfirmationPolicy extends StrolchPolicy {

	public ConfirmationPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
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
}
