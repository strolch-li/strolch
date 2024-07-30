package li.strolch.execution.command;

import static li.strolch.execution.policy.ConfirmationPolicy.DEFAULT_CONFIRMATION;
import static li.strolch.execution.policy.NoPlanning.DEFAULT_PLANNING;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ORDER;

import li.strolch.execution.policy.ConfirmationPolicy;
import li.strolch.execution.policy.PlanningPolicy;
import li.strolch.model.Locator;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.service.api.Command;

public abstract class BasePlanningAndExecutionCommand extends Command {

	public BasePlanningAndExecutionCommand(StrolchTransaction tx) {
		super(tx);
	}

	protected Resource getResource(Action action) {
		return tx().getResourceFor(action, true);
	}

	protected static void updateOrderState(StrolchTransaction tx, Activity rootElement, State currentState,
			State newState) {
		if (currentState == newState)
			return;

		Order order = tx.getOrderByRelation(rootElement, PARAM_ORDER);
		if (order == null) {
			logger.warn("Did not find activity order by relation "
					+ PARAM_ORDER
					+ " for activity {}, trying by Activity type and id", rootElement.getLocator());
			order = tx.getOrderBy(rootElement.getType(), rootElement.getId());
			if (order == null) {
				logger.error("Could not find order by Activity type and id either, not updating order state!");
				return;
			}
		}

		order.setState(rootElement.getState());

		tx.update(order);
	}

	protected ConfirmationPolicy getConfirmationPolicy(Action action) {
		Resource resource = getResource(action);
		PolicyDef policyDef = resource.getPolicyDefs()
				.getPolicyDef(ConfirmationPolicy.class.getSimpleName(), DEFAULT_CONFIRMATION);
		return getComponent(PolicyHandler.class).getPolicy(policyDef, tx());
	}

	protected PlanningPolicy getPlanningPolicy(Action action) {
		PolicyDef planningPolicyDef = action.findPolicy(PlanningPolicy.class, DEFAULT_PLANNING);
		return getComponent(PolicyHandler.class).getPolicy(planningPolicyDef, tx());
	}
}
