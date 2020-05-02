package li.strolch.job;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.privilege.model.PrivilegeContext;

public class ReloadJobsJob extends StrolchJob {

	public ReloadJobsJob(StrolchAgent agent, String id, String name, JobMode mode) {
		super(agent, id, name, mode);
	}

	@Override
	protected void execute(PrivilegeContext ctx) throws Exception {
		StrolchJobsHandler jobsHandler = getContainer().getComponent(StrolchJobsHandler.class);
		jobsHandler.reloadJobs();
	}
}
