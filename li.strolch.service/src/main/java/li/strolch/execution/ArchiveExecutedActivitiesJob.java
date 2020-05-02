package li.strolch.execution;

import java.util.concurrent.TimeUnit;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.execution.command.ArchiveActivityCommand;
import li.strolch.job.JobMode;
import li.strolch.job.StrolchJob;
import li.strolch.model.State;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;

public class ArchiveExecutedActivitiesJob extends StrolchJob {

	public ArchiveExecutedActivitiesJob(StrolchAgent agent, String name, JobMode jobMode) {
		super(agent, name, jobMode);
	}

	public ArchiveExecutedActivitiesJob(StrolchAgent agent, JobMode jobMode, long initialDelay,
			TimeUnit initialDelayTimeUnit, long delay, TimeUnit delayTimeUnit) {
		super(agent, ArchiveExecutedActivitiesJob.class.getSimpleName(), jobMode);
		setDelay(initialDelay, initialDelayTimeUnit, delay, delayTimeUnit);
	}

	@Override
	protected void execute(PrivilegeContext ctx) {

		try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
			tx.streamActivities().forEach(activity -> {
				if (activity.getState() == State.EXECUTED) {
					ArchiveActivityCommand command = new ArchiveActivityCommand(tx);
					command.setActivityLoc(activity.getLocator());
					tx.addCommand(command);
				}
			});

			tx.commitOnClose();
		}
	}
}
