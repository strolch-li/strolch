package li.strolch.model;

import static java.util.Collections.emptyList;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.builder.BuilderHelper.buildParamId;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import li.strolch.model.parameter.Parameter;
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
			setInterpretationAndUom(element, relationP);
			relationsBag().addParameter(relationP);
		}

		relationP.setValue(element.getId());
	}

	@Override
	public void addRelation(StrolchRootElement element) {
		addRelation(buildParamId(element.getType()), element);
	}

	@Override
	public void addRelation(String param, StrolchRootElement element) {
		StringListParameter relationsP = relationsBag().getParameter(param);
		if (relationsP == null) {
			String name = buildParamName(param);
			relationsP = new StringListParameter(param, name, Collections.emptyList());
			setInterpretationAndUom(element, relationsP);
			relationsBag().addParameter(relationsP);
		}

		relationsP.addValueIfNotContains(element.getId());
	}

	private void setInterpretationAndUom(StrolchRootElement element, Parameter<?> relationP) {
		switch (element.getObjectType()) {
		case Tags.RESOURCE -> relationP.setInterpretation(INTERPRETATION_RESOURCE_REF);
		case Tags.ORDER -> relationP.setInterpretation(INTERPRETATION_ORDER_REF);
		case Tags.ACTIVITY -> relationP.setInterpretation(INTERPRETATION_ACTIVITY_REF);
		default -> throw new IllegalStateException("Unexpected value: " + element.getObjectType());
		}

		relationP.setUom(element.getType());
	}

	@Override
	public void setRelations(String param, List<? extends StrolchRootElement> elements) {

		// validate we have same objects
		List<String> objectTypes = elements.stream().map(StrolchRootElement::getObjectType).distinct()
				.toList();
		List<String> types = elements.stream().map(StrolchRootElement::getType).distinct().toList();
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
			case Tags.RESOURCE -> relationsP.setInterpretation(INTERPRETATION_RESOURCE_REF);
			case Tags.ORDER -> relationsP.setInterpretation(INTERPRETATION_ORDER_REF);
			case Tags.ACTIVITY -> relationsP.setInterpretation(INTERPRETATION_ACTIVITY_REF);
			default -> throw new IllegalStateException("Unexpected value: " + objectTypes.get(0));
			}

			relationsP.setUom(types.get(0));

			relationsBag().addParameter(relationsP);
		}

		for (StrolchRootElement element : elements) {
			relationsP.addValueIfNotContains(element.getId());
		}
	}
}
