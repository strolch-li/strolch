package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.FloatParameter;

public class FloatParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<Double, FloatParameter, T> {

	public FloatParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatParameter build() {
		return applyParameter(new FloatParameter());
	}
}
