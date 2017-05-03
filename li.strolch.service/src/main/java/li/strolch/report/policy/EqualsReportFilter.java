package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

public class EqualsReportFilter extends ReportFilterPolicy {

	private boolean negate;
	private String value;

	public EqualsReportFilter(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void init(String value) {
		if (value.startsWith("!")) {
			this.negate = true;
			this.value = value.substring(1);
		} else {
			this.value = value;
		}
	}

	@Override
	public boolean filter(Parameter<?> parameter) {
		String value = parameter.getValueAsString();
		return filter(value);
	}

	@Override
	public boolean filter(String value) {
		if (this.negate)
			return !this.value.equals(value);
		else
			return this.value.equals(value);

	}
}
