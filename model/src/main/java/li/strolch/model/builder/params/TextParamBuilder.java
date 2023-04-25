package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.TextParameter;

public class TextParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<String, TextParameter, T> {

	public TextParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public TextParameter build() {
		return applyParameter(new TextParameter());
	}
}
