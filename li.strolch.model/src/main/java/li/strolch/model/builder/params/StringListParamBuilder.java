package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringListParameter;

public class StringListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<StringListParameter, T> {

	public StringListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public StringListParameter build() {
		return build(new StringListParameter());
	}
}
