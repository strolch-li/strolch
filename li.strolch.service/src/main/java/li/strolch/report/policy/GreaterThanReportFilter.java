package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.parameter.FloatParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;

public class GreaterThanReportFilter extends ReportFilterPolicy {

	private boolean negate;
	private double value;

	public GreaterThanReportFilter(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void init(String value) {
		if (value.startsWith("!")) {
			this.negate = true;
			this.value = Double.parseDouble(value.substring(1));
		} else {
			this.value = Double.parseDouble(value);
		}
	}

	@Override
	public boolean filter(Parameter<?> parameter) {

		FloatParameter floatP = (FloatParameter) parameter;
		if (this.negate)
			return floatP.getValue().doubleValue() <= this.value;
		else
			return floatP.getValue().doubleValue() > this.value;
	}

	@Override
	public boolean filter(String value) {
		throw new UnsupportedOperationException("Greater Than not supported on string!");
	}
}
