package li.strolch.execution.policy;

import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class NoPlanning extends PlanningPolicy {

	public static PolicyDef DEFAULT_PLANNING = PolicyDef
			.valueOf(PlanningPolicy.class.getSimpleName(), "key:DefaultPlanning");

	public NoPlanning(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public Resource evaluateAndSetResource(Action action) {
		tx().lock(Resource.locatorFor(action.getResourceType(), action.getResourceId()));
		return tx().getResourceBy(action.getResourceType(), action.getResourceId());
	}

	@Override
	public void plan(Action action) {
		DBC.PRE.assertEquals("Can not plan illegal state", State.CREATED, action.getState());
		logger.info("Planning action " + action.getLocator());
		action.setState(State.PLANNED);
	}

	@Override
	public void unplan(Action action) {
		DBC.PRE.assertEquals("Can not unplan illegal state", State.PLANNED, action.getState());
		action.setState(State.CREATED);
	}
}
