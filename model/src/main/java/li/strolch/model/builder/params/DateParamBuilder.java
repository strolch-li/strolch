package li.strolch.model.builder.params;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.DateParameter;

public class DateParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Date, DateParameter, T> {

	private ZonedDateTime valueZdt;
	private LocalDateTime valueLdt;

	public DateParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	public ParameterBuilder<Date, DateParameter, T> value(ZonedDateTime value) {
		this.valueZdt = value;
		return this;
	}

	public ParameterBuilder<Date, DateParameter, T> value(LocalDateTime value) {
		this.valueLdt = value;
		return this;
	}

	@Override
	public DateParameter build() {
		DateParameter parameter = new DateParameter();
		build(parameter);
		if (this.valueZdt != null)
			parameter.setValueFromZonedDateTime(this.valueZdt);
		if (this.valueLdt != null)
			parameter.setValueFromLocalDateTime(this.valueLdt);
		return parameter;
	}
}
