package li.strolch.report.policy;

import java.util.Date;

import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.KeyPolicyDef;
import li.strolch.model.policy.PolicyDef;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class ValueRefReportFilter extends ReportFilterPolicy {

	private ReportFilterPolicy filterPolicy;

	public ValueRefReportFilter(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void init(String value) {
		super.init(value);

		this.filterPolicy = tx().getPolicy(PolicyDef
				.valueOf(ReportFilterPolicy.class.getSimpleName(), KeyPolicyDef.XML_PREFIX + this.filterValue));
	}

	@Override
	public boolean filter(Object value) {
		throw new UnsupportedOperationException("2 values required!");
	}

	@Override
	public boolean filter(Object value1, Object value2) {
		DBC.PRE.assertNotNull("value1 required!", value1);
		DBC.PRE.assertNotNull("value2 required!", value2);

		Object left;
		if (value1 instanceof Date) {
			left = value1;
		} else if (value1 instanceof Parameter) {
			Parameter parameter = (Parameter) value1;
			left = parameter.getValue();
		} else {
			left = value1.toString();
		}

		Object right;
		if (value2 instanceof Date) {
			right = value2;
		} else if (value2 instanceof Parameter) {
			Parameter parameter = (Parameter) value2;
			right = parameter.getValue();
		} else {
			right = value2.toString();
		}

		return this.filterPolicy.filter(left, right, this.negate);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		throw new UnsupportedOperationException("Not used");
	}
}
