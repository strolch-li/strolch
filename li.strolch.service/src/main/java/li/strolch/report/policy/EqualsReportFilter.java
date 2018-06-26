package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.ObjectHelper;

public class EqualsReportFilter extends ReportFilterPolicy {

	public EqualsReportFilter(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		if (negate)
			return !ObjectHelper.equals(left, right, false);
		else
			return ObjectHelper.equals(left, right, false);
	}
}
