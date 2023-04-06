package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.DurationParameter;
import li.strolch.utils.time.PeriodDuration;

public class DurationParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<PeriodDuration, DurationParameter, T> {

	public DurationParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public DurationParameter build() {
		return applyParameter(new DurationParameter());
	}
}
