package li.strolch.report.policy;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.ObjectHelper;

public class IsInReportFilter extends ReportFilterPolicy {

	public IsInReportFilter(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		if (negate)
			return !ObjectHelper.isIn(left, right, false);
		else
			return ObjectHelper.isIn(left, right, false);
	}
}
