package li.strolch.model.builder;

import li.strolch.model.ParameterBagContainer;
import li.strolch.model.PolicyContainer;

public class PolicyContainerBuilder<T extends ParameterBagContainerBuilder<T>> extends ParameterBagContainerBuilder<T> {

	private PoliciesBuilder<T> policies;

	public PolicyContainerBuilder(String id, String name, String type) {
		super(id, name, type);
	}

	public PoliciesBuilder<T> policies() {
		if (this.policies == null) {
			@SuppressWarnings("unchecked")
			T t = (T) this;
			this.policies = new PoliciesBuilder<>(t);
		}
		return policies;
	}

	protected void build(PolicyContainer element) {
		build((ParameterBagContainer) element);

		if (this.policies != null)
			this.policies.build(element);
	}
}
