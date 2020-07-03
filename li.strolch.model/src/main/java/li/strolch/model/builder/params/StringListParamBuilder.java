package li.strolch.model.builder.params;

import java.util.List;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringListParameter;

public class StringListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<List<String>, StringListParameter, T> {

	public StringListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	@Override
	public StringListParameter build() {
		return build(new StringListParameter());
	}
}
