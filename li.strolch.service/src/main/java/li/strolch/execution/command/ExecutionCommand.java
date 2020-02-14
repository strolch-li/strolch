package li.strolch.execution.command;

import static li.strolch.execution.policy.NoPlanning.NO_PLANNING;
import static li.strolch.runtime.StrolchConstants.PolicyConstants.PARAM_ORDER;
import static li.strolch.utils.helper.StringHelper.DASH;
import static li.strolch.utils.helper.StringHelper.isEmpty;

import java.util.Iterator;
import java.util.Map.Entry;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.execution.policy.ConfirmationPolicy;
import li.strolch.execution.policy.ExecutionPolicy;
import li.strolch.execution.policy.PlanningPolicy;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.activity.TimeOrderingVisitor;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.visitor.IActivityElementVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.service.api.Command;

public abstract class ExecutionCommand extends Command implements TimeOrderingVisitor, IActivityElementVisitor<Void> {

	public ExecutionCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	protected Locator getResourceLocator(Action action) {
		String resourceId = action.getResourceId();
		if (isEmpty(resourceId) || resourceId.equals(DASH))
			throw new StrolchException("No resourceId defined on action " + action.getLocator());
		String resourceType = action.getResourceType();
		if (isEmpty(resourceType) || resourceType.equals(DASH))
			throw new StrolchException("No resourceType defined on action " + action.getLocator());

		return Resource.locatorFor(resourceType, resourceId);
	}

	protected Resource getResource(Action action) {
		String resourceId = action.getResourceId();
		if (isEmpty(resourceId) || resourceId.equals(DASH))
			throw new StrolchException("No resourceId defined on action " + action.getLocator());
		String resourceType = action.getResourceType();
		if (isEmpty(resourceType) || resourceType.equals(DASH))
			throw new StrolchException("No resourceType defined on action " + action.getLocator());

		return tx().getResourceBy(resourceType, resourceId, true);
	}

	protected static void updateOrderState(StrolchTransaction tx, Activity rootElement, State currentState,
			State newState) {
		if (currentState == newState)
			return;

		Order order = tx.getOrderByRelation(rootElement, PARAM_ORDER);
		if (order == null) {
			logger.warn("Did not find activity order by relation " + PARAM_ORDER + " for activity " + rootElement
					.getLocator() + ", trying by Activity type and id");
			order = tx.getOrderBy(rootElement.getType(), rootElement.getId());
			if (order == null) {
				logger.error("Could not find order by Activity type and id either, not updating order state!");
				return;
			}
		}

		order.setState(rootElement.getState());

		tx.update(order);
	}

	protected ExecutionPolicy getExecutionPolicy(Action action) {
		Resource resource = getResource(action);
		PolicyDef executionPolicyDef = resource.getPolicyDefs().getPolicyDef(ExecutionPolicy.class.getSimpleName());
		return getComponent(PolicyHandler.class).getPolicy(executionPolicyDef, tx());
	}

	protected ConfirmationPolicy getConfirmationPolicy(Action action) {
		Resource resource = getResource(action);
		PolicyDef executionPolicyDef = resource.getPolicyDefs().getPolicyDef(ConfirmationPolicy.class.getSimpleName());
		return getComponent(PolicyHandler.class).getPolicy(executionPolicyDef, tx());
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
	public Void visitActivity(Activity activity) {
		activity.getTimeOrdering().accept(this, activity);
		return null;
	}

	@Override
	public Void visitAction(Action action) {

		// first plan
		if (action.getState().compareTo(State.PLANNED) < 0) {
			PlanningPolicy planningPolicy = tx().getPolicy(action.findPolicy(PlanningPolicy.class, NO_PLANNING));
			planningPolicy.plan(action);
			if (action.getState() != State.PLANNED) {
				logger.info("Action " + action.getLocator() + " was not planned, can thus not executed.");
				return null;
			}
		}

		ExecutionPolicy executionPolicy = getExecutionPolicy(action);

		tx().lock(getResourceLocator(action));

		if (!executionPolicy.isExecutable(action)) {
			logger.info("Action " + action.getLocator() + " is not yet executable.");
			return null;
		}

		logger.info("Action " + action.getLocator() + " is now being executed...");

		// we catch all exceptions because we can't undo, thus need to set the state to ERROR in this case
		// this is only required because we execute actions in same TX as we set to executed any previous actions
		try {
			executionPolicy.toExecution(action);
			getConfirmationPolicy(action).toExecution(action);
		} catch (Exception e) {
			logger.error("Failed to set " + action.getLocator() + " to execution due to " + e.getMessage(), e);
			action.setState(State.ERROR);

			tx().update(action.getRootElement());

			getConfirmationPolicy(action).toError(action);
		}

		return null;
	}
}