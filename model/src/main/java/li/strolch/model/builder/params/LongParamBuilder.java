package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.LongParameter;

public class LongParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Long, LongParameter, T> {

	public LongParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public LongParameter build() {
		return applyParameter(new LongParameter());
	}
}
