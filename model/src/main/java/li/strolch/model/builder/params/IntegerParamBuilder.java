package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.IntegerParameter;

public class IntegerParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Integer, IntegerParameter, T> {

	public IntegerParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerParameter build() {
		return applyParameter(new IntegerParameter());
	}
}
