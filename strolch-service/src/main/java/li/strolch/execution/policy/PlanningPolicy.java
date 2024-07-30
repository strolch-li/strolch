package li.strolch.execution.policy;

import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class PlanningPolicy extends StrolchPolicy {

	public static final PolicyDef DEFAULT_PLANNING = PolicyDef.getKeyPolicy(PlanningPolicy.class, "DefaultPlanning");

	public PlanningPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public Resource evaluateAndSetResource(Action action) {
		if (!action.isResourceDefined())
			return null;

		tx().lock(action.getResourceLocator());
		return tx().getResourceBy(action.getResourceType(), action.getResourceId());
	}

	public abstract void plan(Action action);

	public abstract void unplan(Action action);
}
