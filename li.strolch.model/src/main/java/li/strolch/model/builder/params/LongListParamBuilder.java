package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.LongListParameter;

public class LongListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<LongListParameter, T> {

	public LongListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public LongListParameter build() {
		return build(new LongListParameter());
	}
}
