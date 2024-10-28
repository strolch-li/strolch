package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.BooleanParameter;

public class BooleanParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Boolean, BooleanParameter, T> {

	public BooleanParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public BooleanParameter build() {
		return applyParameter(new BooleanParameter());
	}
}
