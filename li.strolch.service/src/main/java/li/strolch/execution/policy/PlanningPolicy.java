package li.strolch.execution.policy;

import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class PlanningPolicy extends StrolchPolicy {

	public static PolicyDef DEFAULT_PLANNING = PolicyDef
			.valueOf(PlanningPolicy.class.getSimpleName(), "key:DefaultPlanning");

	public PlanningPolicy(StrolchTransaction tx) {
		super(tx);
	}

	public abstract Resource evaluateAndSetResource(Action action);

	public abstract void plan(Action action);

	public abstract void unplan(Action action);

	@Override
	public void undo() {
		// do nothing
	}
}
