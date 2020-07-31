package li.strolch.model.builder.params;

import static li.strolch.model.StrolchModelConstants.*;

import li.strolch.model.builder.BagBuilder;
import li.strolch.model.builder.ParameterBagContainerBuilder;
import li.strolch.model.parameter.StringParameter;

public class StringParamBuilder<T extends ParameterBagContainerBuilder<T>>
		extends ParameterBuilder<String, StringParameter, T> {

	public StringParamBuilder(BagBuilder<T> builder, String id, String name) {
		super(builder, id, name);
	}

	public StringParamBuilder<T> resourceRef(String type) {
		this.interpretation = INTERPRETATION_RESOURCE_REF;
		this.uom = type;
		return this;
	}

	public StringParamBuilder<T> orderRef(String type) {
		this.interpretation = INTERPRETATION_ORDER_REF;
		this.uom = type;
		return this;
	}

	public StringParamBuilder<T> activityRef(String type) {
		this.interpretation = INTERPRETATION_ACTIVITY_REF;
		this.uom = type;
		return this;
	}

	@Override
	public StringParameter build() {
		return build(new StringParameter());
	}
}
