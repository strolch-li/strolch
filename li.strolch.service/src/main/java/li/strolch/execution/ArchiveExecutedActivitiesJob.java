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

	public ArchiveExecutedActivitiesJob(StrolchAgent agent) {
		super(agent, JobMode.Manual, 0, TimeUnit.MINUTES, 0, TimeUnit.MINUTES);
	}

	public ArchiveExecutedActivitiesJob(StrolchAgent agent, JobMode jobMode, long initialDelay,
			TimeUnit initialDelayTimeUnit, long delay, TimeUnit delayTimeUnit) {
		super(agent, jobMode, initialDelay, initialDelayTimeUnit, delay, delayTimeUnit);
	}

	@Override
	protected void execute(PrivilegeContext ctx) {

		try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
			tx.streamActivities().forEach(activity -> {
				if (activity.getState() == State.EXECUTED) {
					ArchiveActivityCommand command = new ArchiveActivityCommand(tx);
					command.setActivity(activity);
					tx.addCommand(command);
				}
			});

			tx.commitOnClose();
		}
	}
}
