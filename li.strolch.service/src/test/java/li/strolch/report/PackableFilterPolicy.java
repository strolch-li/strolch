package li.strolch.report;

import li.strolch.model.Resource;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.report.policy.ReportFilterPolicy;
import li.strolch.utils.dbc.DBC;

public class PackableFilterPolicy extends ReportFilterPolicy {

	public PackableFilterPolicy(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public boolean filter(Object value) {
		throw new UnsupportedOperationException("2 values required!");
	}

	@Override
	public boolean filter(Object value1, Object value2) {
		DBC.PRE.assertNotNull("value1 required!", value1);
		DBC.PRE.assertNotNull("value2 required!", value2);

		BooleanParameter productPackable = (BooleanParameter) value1;
		Resource location = (Resource) value2;
		BooleanParameter locationPackable = location.getParameter("packingLocation", false);
		if (locationPackable == null)
			return this.negate;
		boolean packable = productPackable.getValue() == locationPackable.getValue();
		return this.negate != packable;
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		throw new UnsupportedOperationException("Not used");
	}
}
