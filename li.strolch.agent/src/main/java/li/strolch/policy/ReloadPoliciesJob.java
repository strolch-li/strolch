package li.strolch.policy;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.job.StrolchJob;
import li.strolch.privilege.model.PrivilegeContext;

public class ReloadPoliciesJob extends StrolchJob {

	public ReloadPoliciesJob(StrolchAgent agent) {
		super(agent);
	}

	@Override
	protected void execute(PrivilegeContext ctx) throws Exception {
		PolicyHandler policyHandler = getContainer().getComponent(PolicyHandler.class);
		policyHandler.reloadPolicies();
	}
}
