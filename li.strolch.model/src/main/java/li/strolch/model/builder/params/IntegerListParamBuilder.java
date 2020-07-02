package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.IntegerListParameter;

public class IntegerListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<IntegerListParameter, T> {

	public IntegerListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerListParameter build() {
		return build(new IntegerListParameter());
	}
}
