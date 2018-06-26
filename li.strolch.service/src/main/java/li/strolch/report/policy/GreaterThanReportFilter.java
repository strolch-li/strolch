package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.ObjectHelper;

public class GreaterThanReportFilter extends ReportFilterPolicy {

	public GreaterThanReportFilter(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		if (negate)
			return ObjectHelper.compare(left, right, false) > 0;
		else
			return ObjectHelper.compare(right, left, false) > 0;
	}
}
