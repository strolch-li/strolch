package li.strolch.model.builder.params;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringListParameter;

import java.util.List;

import static li.strolch.model.StrolchModelConstants.*;

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
		return applyParameter(new StringListParameter());
	}
}
