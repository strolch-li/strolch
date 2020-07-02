package li.strolch.model.builder;

import li.strolch.model.PolicyContainer;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;

public class PoliciesBuilder<T extends ParameterBagContainerBuilder<T>> {

	private final T builder;
	private final PolicyDefs policyDefs;

	public PoliciesBuilder(T builder) {
		this.builder = builder;
		this.policyDefs = new PolicyDefs();
	}

	public PoliciesBuilder<T> planning(String value) {
		return policy("PlanningPolicy", value);
	}

	public PoliciesBuilder<T> execution(String value) {
		return policy("ExecutionPolicy", value);
	}

	public PoliciesBuilder<T> confirmation(String value) {
		return policy("ConfirmationPolicy", value);
	}

	public PoliciesBuilder<T> policy(String type, String value) {
		this.policyDefs.addOrUpdate(PolicyDef.valueOf(type, value));
		return this;
	}

	public T endPolicies() {
		return builder;
	}

	public void build(PolicyContainer element) {
		element.setPolicyDefs(this.policyDefs.getClone());
	}
}
