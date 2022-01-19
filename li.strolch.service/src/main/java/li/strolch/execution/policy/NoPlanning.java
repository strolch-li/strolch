package li.strolch.execution.policy;

import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class NoPlanning extends PlanningPolicy {

	public NoPlanning(StrolchTransaction tx) {
		super(tx);
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
