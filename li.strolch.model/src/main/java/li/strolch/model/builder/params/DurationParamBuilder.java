package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.DurationParameter;

public class DurationParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<DurationParameter, T> {

	public DurationParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public DurationParameter build() {
		return build(new DurationParameter());
	}
}
