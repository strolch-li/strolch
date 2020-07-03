package li.strolch.model.builder.params;

import java.util.List;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.FloatListParameter;

public class FloatListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<List<Double>, FloatListParameter, T> {

	public FloatListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public FloatListParameter build() {
		return build(new FloatListParameter());
	}
}
