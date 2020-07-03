package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringParameter;

public class StringParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<String, StringParameter, T> {

	public StringParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public StringParameter build() {
		return build(new StringParameter());
	}
}
