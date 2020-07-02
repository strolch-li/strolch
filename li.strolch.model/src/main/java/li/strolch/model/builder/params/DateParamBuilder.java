package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.DateParameter;

public class DateParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<DateParameter, T> {

	public DateParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public DateParameter build() {
		return build(new DateParameter());
	}
}
