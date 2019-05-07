package li.strolch.policy;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.job.StrolchJob;
import li.strolch.privilege.model.PrivilegeContext;

public class ReloadPrivilegeHandlerJob extends StrolchJob {

	public ReloadPrivilegeHandlerJob(StrolchAgent agent) {
		super(agent);
	}

	@Override
	protected void execute(PrivilegeContext ctx) throws Exception {
		getContainer().getPrivilegeHandler().reloadConfiguration();
	}
}
