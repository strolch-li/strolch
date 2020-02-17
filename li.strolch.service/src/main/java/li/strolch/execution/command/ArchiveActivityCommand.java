package li.strolch.execution.command;

import li.strolch.execution.policy.ActivityArchivalPolicy;
import li.strolch.model.Locator;
import li.strolch.model.activity.Activity;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.PolicyHandler;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

public class ArchiveActivityCommand extends Command {

	private static final String KEY_DEFAULT_ACTIVITY_ARCHIVAL = "key:DefaultActivityArchival";

	private Locator activityLoc;

	public ArchiveActivityCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setActivityLoc(Locator activityLoc) {
		this.activityLoc = activityLoc;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("activity can not be null!", this.activityLoc);
	}

	@Override
	public void doCommand() {
		tx().lock(this.activityLoc);

		Activity activity = tx().getActivityBy(this.activityLoc.get(1), this.activityLoc.get(2));
		if (activity == null) {
			logger.error("Activity " + this.activityLoc + " does not exist anymore, can not archive!");
			return;
		}

		logger.info("Activity " + activity.getLocator() + " is in state " + activity.getState());

		PolicyDef policyDef;
		if (activity.hasPolicyDef(ActivityArchivalPolicy.class.getSimpleName())) {
			policyDef = activity.getPolicyDef(ActivityArchivalPolicy.class.getSimpleName());
		} else {
			policyDef = PolicyDef.valueOf(ActivityArchivalPolicy.class.getSimpleName(), KEY_DEFAULT_ACTIVITY_ARCHIVAL);
		}

		PolicyHandler policyHandler = getComponent(PolicyHandler.class);
		ActivityArchivalPolicy archivalPolicy = policyHandler.getPolicy(policyDef, tx());
		archivalPolicy.archive(activity);
	}
}
