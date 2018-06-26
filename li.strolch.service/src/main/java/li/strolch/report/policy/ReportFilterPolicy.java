package li.strolch.report.policy;

import java.util.Date;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.policy.StrolchPolicy;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public abstract class ReportFilterPolicy extends StrolchPolicy {

	public ReportFilterPolicy(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	private boolean negate;
	private String filterValue;
	private Object left;

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
				this.left = ISO8601FormatFactory.getInstance().parseDate(this.filterValue);
			right = value;

		} else if (value instanceof Parameter) {

			Parameter parameter = (Parameter) value;
			if (this.left == null)
				this.left = parameter.getValueType().parseValue(this.filterValue);
			right = parameter.getValue();

		} else {
			if (this.left == null)
				this.left = this.filterValue;
			right = value.toString();
		}

		return filter(this.left, right, this.negate);
	}

	protected abstract boolean filter(Object left, Object right, boolean negate);

	@Override
	public void undo() {
		// do nothing
	}
}
