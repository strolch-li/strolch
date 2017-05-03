package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;

public abstract class ReportFilterPolicy extends StrolchPolicy {

	public ReportFilterPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	public abstract void init(String value);

	public abstract boolean filter(String value);

	public abstract boolean filter(Parameter<?> param);

	@Override
	public void undo() {
		// do nothing
	}
}
