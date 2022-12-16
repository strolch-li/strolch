package li.strolch.report.policy;

import static java.util.stream.Collectors.toList;

import java.util.Arrays;
import java.util.Date;

import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.DateParameter;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.ObjectHelper;
import li.strolch.utils.dbc.DBC;

public class IsInReportFilter extends ReportFilterPolicy {

	public IsInReportFilter(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public boolean filter(Object value) {
		DBC.PRE.assertNotNull("value required!", value);

		Object left;
		if (value instanceof Date) {

			if (this.right == null) {
				logger.error("DEPRECATED, use ZonedDateTime");
				this.right = Arrays.stream(this.filterValue.split(",")).map(String::trim)
						.map(this::parseFilterValueToDate).collect(toList());
			}

			left = value;

		} else if (value instanceof Parameter) {

			Parameter<?> parameter = (Parameter<?>) value;
			if (this.right == null) {
				StrolchValueType valueType = parameter.getValueType();
				if (valueType == StrolchValueType.DATE)
					this.right = Arrays.stream(this.filterValue.split(",")).map(String::trim)
							.map(this::parseFilterValueToZdt).collect(toList());
				else
					this.right = Arrays.stream(this.filterValue.split(",")).map(String::trim).map(valueType::parseValue)
							.collect(toList());
			}

			if (value instanceof DateParameter)
				left = ((DateParameter) parameter).getValueZdt();
			else
				left = parameter.getValue();

		} else {
			if (this.right == null)
				this.right = Arrays.stream(this.filterValue.split(",")).map(String::trim).collect(toList());

			left = value.toString();
		}

		return filter(left, this.right, this.negate);
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		if (negate)
			return !ObjectHelper.isIn(left, right, false);
		else
			return ObjectHelper.isIn(left, right, false);
	}
}
