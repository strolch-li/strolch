package li.strolch.model;

import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.builder.BuilderHelper.buildParamId;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

import li.strolch.model.parameter.StringParameter;

public abstract class AbstractStrolchRootElement extends GroupedParameterizedElement implements StrolchRootElement {

	public AbstractStrolchRootElement() {
		super();
	}

	public AbstractStrolchRootElement(String id, String name, String type) {
		super(id, name, type);
	}

	public void setRelation(StrolchRootElement element) {
		setRelation(buildParamId(element.getType()), element);
	}

	public void setRelation(String param, StrolchRootElement element) {
		StringParameter relationP = relationsBag().getParameter(param);
		if (relationP == null) {
			String name = buildParamName(param);
			relationP = new StringParameter(param, name, "");

			switch (element.getObjectType()) {
			case Tags.RESOURCE:
				relationP.setInterpretation(INTERPRETATION_RESOURCE_REF);
				break;
			case Tags.ORDER:
				relationP.setInterpretation(INTERPRETATION_ORDER_REF);
				break;
			case Tags.ACTIVITY:
				relationP.setInterpretation(INTERPRETATION_ACTIVITY_REF);
				break;
			}

			relationP.setUom(element.getType());

			relationsBag().addParameter(relationP);
		}

		relationP.setValue(element.getId());
	}
}
