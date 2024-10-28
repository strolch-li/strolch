package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.IntegerListParameter;

import java.util.List;

public class IntegerListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<List<Integer>, IntegerListParameter, T> {

	public IntegerListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public IntegerListParameter build() {
		return applyParameter(new IntegerListParameter());
	}
}
