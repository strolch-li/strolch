package li.strolch.policy;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.job.JobMode;
import li.strolch.job.StrolchJob;
import li.strolch.privilege.model.PrivilegeContext;

public class ReloadPoliciesJob extends StrolchJob {

	public ReloadPoliciesJob(StrolchAgent agent, String id, String name, JobMode mode) {
		super(agent, id, name, mode);
	}

	@Override
	protected void execute(PrivilegeContext ctx) {
		PolicyHandler policyHandler = getContainer().getComponent(PolicyHandler.class);
		policyHandler.reloadPolicies();
	}
}
