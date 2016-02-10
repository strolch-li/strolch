package li.strolch.model;

import li.strolch.model.policy.PolicyDefs;

public interface PolicyContainer {

	public PolicyDefs getPolicyDefs();

	public boolean hasPolicyDefs();

	public void setPolicyDefs(PolicyDefs policyDefs);
}
