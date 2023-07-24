package li.strolch.execution.command;

import static li.strolch.model.State.EXECUTION;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ATOMIC_PARALLEL_EXECUTION;

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
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.model.visitor.IActivityElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class PlanAndExecuteActivityCommand extends BasePlanningAndExecutionCommand
		implements TimeOrderingVisitor, IActivityElementVisitor<Void> {

	private Controller controller;
	private boolean needsRetriggerOfExecution;

	public PlanAndExecuteActivityCommand(StrolchTransaction tx) {
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
	public Void visitActivity(Activity activity) {
		activity.getTimeOrdering().accept(this, activity);
		return null;
	}

	@Override
	public Void visitAction(Action action) {
		planAndExecute(action);
		return null;
	}

	private void planAndExecute(Action action) {
		if (planAction(action))
			executeAction(action);
	}

	private void executeAction(Action action) {
		State currentState = action.getState();
		if (currentState.compareTo(State.EXECUTED) >= 0 || currentState == EXECUTION)
			return;
		if (currentState == State.CREATED || currentState == State.PLANNING)
			throw new IllegalStateException("Action " + action.getLocator() + " is in illegal state " + currentState);

		if (!currentState.canSetToExecution()) {
			logger.warn("Action " + action.getLocator() + " can not be executed with state " + currentState);
			return;
		}

		tx().lock(action.getResourceLocator());

		ConfirmationPolicy confirmationPolicy = getConfirmationPolicy(action);
		ExecutionPolicy executionPolicy = getExecutionPolicy(action);

		// we catch all exceptions because we can't undo, thus need to set the state to ERROR in this case
		// this is only required because we execute actions in same TX as we set to-executed any previous actions
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
			confirmationPolicy.doConfirmation(action);

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

	private boolean planActivity(Activity activity) {
		PlanActivityCommand planning = new PlanActivityCommand(tx());
		planning.setActivity(activity);
		planning.validateAndDoCommand();
		return activity.getState().compareTo(State.PLANNED) >= 0;
	}

	private void unplanActivity(Activity activity) {
		UnplanActivityCommand planning = new UnplanActivityCommand(tx());
		planning.setActivity(activity);
		planning.validateAndDoCommand();
	}

	private boolean planAction(Action action) {
		State currentState = action.getState();
		if (currentState.compareTo(State.PLANNED) >= 0)
			return true;

		getPlanningPolicy(action).plan(action);
		if (action.getState().compareTo(State.PLANNED) >= 0) {
			// planning is complete, so we can now confirm it
			getConfirmationPolicy(action).toPlanned(action);
			return true;
		}

		if (currentState != action.getState() && action.isResourceDefined())
			getConfirmationPolicy(action).doConfirmation(action);
		logger.info("Failed to plan action " + action.getLocator() + ", thus no execution possible");
		return false;
	}

	@Override
	public void visitSeries(Activity activity) {
		if (activity.inClosedPhase())
			return;

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			State state = element.getState();
			if (element.inClosedPhase())
				continue;

			// in series, we can never have two Actions in execution, so if we found the action in execution, we stop
			if (element instanceof Action && state.inExecutionPhase() && state != State.STOPPED)
				break;

			// in series, we stop when the first action is set to execution
			element.accept(this);
			break;
		}
	}

	@Override
	public void visitParallel(Activity activity) {
		if (activity.inClosedPhase())
			return;

		BooleanParameter atomicExecutionP = activity.findParameter(PARAM_ATOMIC_PARALLEL_EXECUTION, false);
		boolean atomicExecution = atomicExecutionP != null && atomicExecutionP.getValue();

		if (atomicExecution) {
			if (!planActivity(activity)) {
				unplanActivity(activity);
				return;
			}

			// stop execution if at least one action is not executable from this entire tree
			boolean anyActionNotExecutable = activity.streamActionsDeep().anyMatch(a -> {
				if (!a.getState().canSetToExecution())
					return false;
				ExecutionPolicy executionPolicy = getExecutionPolicy(a);
				executionPolicy.initialize(a);
				boolean executable = executionPolicy.isExecutable(a);
				if (!executable)
					logger.info("Action " + a.getLocator() + " is not executable yet!");
				return !executable;
			});
			if (anyActionNotExecutable)
				return;
		}

		Iterator<Entry<String, IActivityElement>> iter = activity.elementIterator();
		while (iter.hasNext()) {
			IActivityElement element = iter.next().getValue();
			if (element.inClosedPhase())
				continue;

			// in parallel, we execute all the actions in the activity
			element.accept(this);
		}
	}
}
