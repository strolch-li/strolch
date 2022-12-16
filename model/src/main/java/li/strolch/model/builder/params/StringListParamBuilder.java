package li.strolch.model.builder.params;

import static li.strolch.model.StrolchModelConstants.*;

import java.util.List;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringListParameter;

public class StringListParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<List<String>, StringListParameter, T> {

	public StringListParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	public StringListParamBuilder<T> resourceRef(String type) {
		this.interpretation = INTERPRETATION_RESOURCE_REF;
		this.uom = type;
		return this;
	}

	public StringListParamBuilder<T> orderRef(String type) {
		this.interpretation = INTERPRETATION_ORDER_REF;
		this.uom = type;
		return this;
	}

	public StringListParamBuilder<T> activityRef(String type) {
		this.interpretation = INTERPRETATION_ACTIVITY_REF;
		this.uom = type;
		return this;
	}

	@Override
	public StringListParameter build() {
		return build(new StringListParameter());
	}
}
