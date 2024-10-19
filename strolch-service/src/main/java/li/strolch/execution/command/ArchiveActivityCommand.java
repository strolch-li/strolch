package li.strolch.execution.command;

import li.strolch.execution.policy.ActivityArchivalPolicy;
import li.strolch.model.Locator;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

public class ArchiveActivityCommand extends Command {

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
			logger.error("Activity {} does not exist anymore, can not archive!", this.activityLoc);
			return;
		}

		logger.debug("Archiving activity {}", activity.getLocator());
		tx().getPolicy(activity, ActivityArchivalPolicy.class).archive(activity);
	}
}
