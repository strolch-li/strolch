package li.strolch.report.policy;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.ObjectHelper;

public class ContainsReportFilter extends ReportFilterPolicy {

	public ContainsReportFilter(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		if (negate)
			return !ObjectHelper.contains(left, right, false);
		else
			return ObjectHelper.contains(left, right, false);
	}
}
