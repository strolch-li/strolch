package li.strolch.report.policy;

import java.time.LocalDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.util.Date;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public abstract class ReportFilterPolicy extends StrolchPolicy {

	public ReportFilterPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	protected boolean negate;
	protected String filterValue;
	protected Object left;

	public boolean isNegate() {
		return this.negate;
	}

	public void setNegate(boolean negate) {
		this.negate = negate;
	}

	public void setFilterValue(String filterValue) {
		this.filterValue = filterValue;
	}

	public Object getLeft() {
		return this.left;
	}

	public void setLeft(Object left) {
		this.left = left;
	}

	public void init(String value) {
		if (value.startsWith("!")) {
			setNegate(true);
			setFilterValue(value.substring(1));
		} else {
			setFilterValue(value);
		}
	}

	public boolean filter(Object value) {

		Object right;
		if (value instanceof Date) {

			if (this.left == null)
				this.left = parseFilterValueToDate(this.filterValue);

			right = value;

		} else if (value instanceof Parameter) {

			Parameter parameter = (Parameter) value;
			if (this.left == null) {
				StrolchValueType valueType = parameter.getValueType();
				if (valueType == StrolchValueType.DATE)
					this.left = parseFilterValueToDate(this.filterValue);
				else
					this.left = valueType.parseValue(this.filterValue);
			}

			right = parameter.getValue();

		} else {
			if (this.left == null)
				this.left = this.filterValue;

			right = value.toString();
		}

		return filter(this.left, right, this.negate);
	}

	private Date parseFilterValueToDate(String filterValue) {
		DBC.INTERIM.assertNotEmpty("filterValue must not be empty for date comparisons!", filterValue);

		if (!filterValue.startsWith("now"))
			return ISO8601FormatFactory.getInstance().parseDate(filterValue);

		if (filterValue.charAt(3) != '(' || filterValue.charAt(filterValue.length() - 1) != ')')
			throw new IllegalArgumentException("now() format invalid for " + filterValue);

		String periodS = filterValue.substring(4, filterValue.length() - 1);
		if (periodS.isEmpty())
			return new Date();

		Period period = Period.parse(periodS);
		LocalDateTime dateTime = LocalDateTime.now().plus(period);
		return Date.from(dateTime.atZone(ZoneId.systemDefault()).toInstant());
	}

	protected abstract boolean filter(Object left, Object right, boolean negate);

	@Override
	public void undo() {
		// do nothing
	}
}
