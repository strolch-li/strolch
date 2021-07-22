package li.strolch.model;

import static java.util.Collections.emptyList;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

import java.util.List;
import java.util.stream.Collectors;

import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

public abstract class AbstractStrolchRootElement extends GroupedParameterizedElement implements StrolchRootElement {

	public AbstractStrolchRootElement() {
		super();
	}

	public AbstractStrolchRootElement(String id, String name, String type) {
		super(id, name, type);
	}

	@Override
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

	@Override
	public void setRelations(String param, List<? extends StrolchRootElement> elements) {

		// validate we have same objects
		List<String> objectTypes = elements.stream().map(StrolchRootElement::getObjectType).distinct()
				.collect(Collectors.toList());
		List<String> types = elements.stream().map(StrolchRootElement::getType).distinct().collect(Collectors.toList());
		if (objectTypes.size() != 1)
			throw new IllegalStateException(
					"Only allow to have one type of object: " + elements.stream().map(StrolchElement::getId)
							.collect(Collectors.joining(", ")));
		if (types.size() != 1)
			throw new IllegalStateException(
					"Only allow to have one type of object: " + elements.stream().map(StrolchElement::getId)
							.collect(Collectors.joining(", ")));

		StringListParameter relationsP = relationsBag().getParameter(param);
		if (relationsP == null) {
			String name = buildParamName(param);
			relationsP = new StringListParameter(param, name, emptyList());

			switch (objectTypes.get(0)) {
			case Tags.RESOURCE:
				relationsP.setInterpretation(INTERPRETATION_RESOURCE_REF);
				break;
			case Tags.ORDER:
				relationsP.setInterpretation(INTERPRETATION_ORDER_REF);
				break;
			case Tags.ACTIVITY:
				relationsP.setInterpretation(INTERPRETATION_ACTIVITY_REF);
				break;
			}

			relationsP.setUom(types.get(0));

			relationsBag().addParameter(relationsP);
		}

		for (StrolchRootElement element : elements) {
			relationsP.addValueIfNotContains(element.getId());
		}
	}
}
