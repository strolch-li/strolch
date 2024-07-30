package li.strolch.execution.policy;

import static li.strolch.model.StrolchModelConstants.PolicyConstants.PARAM_ORDER;

import li.strolch.model.Order;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

public class SimpleActivityCreationPolicy extends ActivityCreationPolicy {
	public SimpleActivityCreationPolicy(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public Activity create(Order order) {

		// create a new activity by using the template
		Activity activity = tx().getActivityTemplate(order.getType());

		activity.setName(order.getName());
		activity.setRelationId(PARAM_ORDER, order.getId());

		tx().add(activity);

		return activity;
	}
}
