package li.strolch.execution.command;

import java.util.Iterator;
import java.util.Map.Entry;

import li.strolch.execution.Controller;
import li.strolch.execution.policy.ConfirmationPolicy;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.activity.TimeOrderingVisitor;
import li.strolch.model.visitor.IActivityElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class ExecuteActivityCommand extends BasePlanningAndExecutionCommand
		implements TimeOrderingVisitor, IActivityElementVisitor<Void> {

	private Controller controller;
	private boolean needsRetriggerOfExecution;

	public ExecuteActivityCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setController(Controller controller) {
		this.controller = controller;
	}

	private ExecutionPolicy getExecutionPolicy(Action action) {
		return this.controller.refreshExecutionPolicy(tx(), action);
	}

	public boolean needsRetriggerOfExecution() {
		return this.needsRetriggerOfExecution;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("controller can not be null!", this.controller);
	}

	@Override
	public void doCommand() {
		Activity activity = this.controller.getActivity();
		State currentState = activity.getState();
		activity.accept(this);
		updateOrderState(tx(), activity, currentState, activity.getState());
	}

	@Override
	public Void visitAction(Action action) {
		execute(action);
		return null;
	}

	public void execute(Action action) {

		// first plan
		if (action.getState().compareTo(State.PLANNED) < 0) {
			getPlanningPolicy(action).plan(action);
			if (action.getState() != State.PLANNED) {
				logger.info("Action " + action.getLocator() + " was not planned, can thus not executed.");
				return;
			}
		}

		tx().lock(action.getResourceLocator());
		tx().removeFromCache(action.getResourceLocator());

		ConfirmationPolicy confirmationPolicy = getConfirmationPolicy(action);
		ExecutionPolicy executionPolicy = getExecutionPolicy(action);

		// we catch all exceptions because we can't undo, thus need to set the state to ERROR in this case
		// this is only required because we execute actions in same TX as we set to executed any previous actions
		try {

			if (executionPolicy.isStopped()) {
				this.controller.removeExecutionPolicy(action);
				executionPolicy = getExecutionPolicy(action);
			}

			executionPolicy.initialize(action);
			if (!executionPolicy.isExecutable(action)) {
				logger.info("Action " + action.getLocator() + " is not yet executable.");
				return;
			}

			logger.info("Action " + action.getLocator() + " is now being executed...");

			executionPolicy.toExecution(action);
			confirmationPolicy.toExecution(action);

			if (action.getState() == State.EXECUTED)
				this.needsRetriggerOfExecution = true;

		} catch (Exception e) {
			logger.error("Failed to set " + action.getLocator() + " to execution due to " + e.getMessage(), e);

			try {
				executionPolicy.stop();
			} catch (Exception ex) {
				logger.error("Failed to stop execution policy for " + action.getLocator(), e);
			}

			action.setState(State.ERROR);
			tx().update(action.getRootElement());

			confirmationPolicy.toError(action);
		}
	}

	protected boolean isExecutable(IActivityElement element) {
		State state = element.getState();
		if (state.compareTo(State.EXECUTED) >= 0)
			return false;

		if (element instanceof Activity)
			return true;

		// not yet in execution
		if (state.compareTo(State.EXECUTION) < 0)
			return true;

		// in stopped, means we can re-execute
		if (state == State.STOPPED)
			return true;

		// if in ERROR, then must first be handled
		return false;
	}

	@Override
	public void visitSeries(Activity activity) {

		if (activity.getState().compareTo(State.EXECUTED) >= 0)
			return;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			State state = element.getState();
			if (element.getState().compareTo(State.EXECUTED) >= 0)
				continue;

			// in series we can never have two Actions in execution, so if we found the action in execution, we stop
			if (element instanceof Action //
					&& (state == State.EXECUTION //
					|| state == State.WARNING //
					|| state == State.ERROR)) {
				break;
			}

			boolean canExecute = isExecutable(element);
			if (canExecute) {
				element.accept(this);

				// in series we stop when the first action is set to execution
				break;
			}
		}
	}

	@Override
	public void visitParallel(Activity activity) {

		if (activity.getState().compareTo(State.EXECUTED) >= 0)
			return;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			if (element.getState().isExecuted())
				continue;

			// in parallel we execute all the actions in the activity

			boolean canExecute = isExecutable(element);
			if (canExecute) {
				element.accept(this);
			}
		}
	}

	@Override
	public Void visitActivity(Activity activity) {
		activity.getTimeOrdering().accept(this, activity);
		return null;
	}
}
