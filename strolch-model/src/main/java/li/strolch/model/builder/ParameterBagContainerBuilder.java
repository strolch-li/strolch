package li.strolch.model.builder;

import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterBagContainer;
import li.strolch.model.Tags;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;

import java.util.HashMap;
import java.util.Map;

import static java.util.Collections.emptyList;
import static li.strolch.model.StrolchModelConstants.*;
import static li.strolch.model.StrolchModelConstants.PolicyConstants.BAG_OBJECTIVES;
import static li.strolch.model.builder.BuilderHelper.buildParamId;
import static li.strolch.model.builder.BuilderHelper.buildParamName;

public abstract class ParameterBagContainerBuilder<T extends ParameterBagContainerBuilder<T>> {

	private final String id;
	private final String type;
	private final String name;

	private final Map<String, BagBuilder<T>> parametersBags;
	private final Map<String, String[]> singleRelations;
	private final Map<String, String[]> multiRelations;

	public ParameterBagContainerBuilder(String id, String type) {
		this(id, buildParamName(id), type);
	}

	public ParameterBagContainerBuilder(String id, String name, String type) {
		this.id = id;
		this.name = name;
		this.type = type;

		this.parametersBags = new HashMap<>();
		this.singleRelations = new HashMap<>();
		this.multiRelations = new HashMap<>();
	}

	public String getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public String getType() {
		return this.type;
	}

	public BagBuilder<T> defaultBag() {
		return bag(BAG_PARAMETERS, TYPE_PARAMETERS, TYPE_PARAMETERS);
	}

	public BagBuilder<T> objectivesBag() {
		return bag(BAG_OBJECTIVES, TYPE_OBJECTIVES, TYPE_OBJECTIVES);
	}

	public BagBuilder<T> relationsBag() {
		return bag(BAG_RELATIONS, TYPE_RELATIONS, TYPE_RELATIONS);
	}

	public BagBuilder<T> bag(String id, String type) {
		return bag(id, buildParamName(id), type);
	}

	public BagBuilder<T> bag(String id, String name, String type) {
		@SuppressWarnings("unchecked") BagBuilder<T> bagBuilder = new BagBuilder<>((T) this, id, name, type);
		if (this.parametersBags.put(id, bagBuilder) != null)
			throw new IllegalArgumentException("Bag builder for " + id + " already exists!");
		return bagBuilder;
	}

	public T resourceRelation(String type) {
		return resourceRelation(buildParamId(type), type, type);
	}

	public T resourceRelation(String paramId, String type) {
		return resourceRelation(paramId, buildParamName(paramId), type);
	}

	public T resourceRelation(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.singleRelations.put(paramId, new String[]{paramName, type, Tags.RESOURCE});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	public T resourceRelations(String type) {
		return resourceRelations(buildParamId(type), type + "s", type);
	}

	public T resourceRelations(String paramId, String type) {
		return resourceRelations(paramId, buildParamName(paramId), type);
	}

	public T resourceRelations(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.multiRelations.put(paramId, new String[]{paramName, type, Tags.RESOURCE});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	public T orderRelation(String type) {
		return orderRelation(buildParamId(type), type, type);
	}

	public T orderRelation(String paramId, String type) {
		return orderRelation(paramId, buildParamName(paramId), type);
	}

	public T orderRelation(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.singleRelations.put(paramId, new String[]{paramName, type, Tags.ORDER});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	public T orderRelations(String type) {
		return orderRelations(buildParamId(type), type + "s", type);
	}

	public T orderRelations(String paramId, String type) {
		return orderRelations(paramId, buildParamName(paramId), type);
	}

	public T orderRelations(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.multiRelations.put(paramId, new String[]{paramName, type, Tags.ORDER});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	public T activityRelation(String type) {
		return activityRelation(buildParamId(type), type, type);
	}

	public T activityRelation(String paramId, String type) {
		return activityRelation(paramId, buildParamName(paramId), type);
	}

	public T activityRelation(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.singleRelations.put(paramId, new String[]{paramName, type, Tags.ACTIVITY});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	public T activityRelations(String type) {
		return activityRelations(buildParamId(type), type + "s", type);
	}

	public T activityRelations(String paramId, String type) {
		return activityRelations(paramId, buildParamName(paramId), type);
	}

	public T activityRelations(String paramId, String paramName, String type) {
		assertNotMapped(paramId);
		this.multiRelations.put(paramId, new String[]{paramName, type, Tags.ACTIVITY});
		@SuppressWarnings("unchecked") T t = (T) this;
		return t;
	}

	private void assertNotMapped(String paramId) {
		if (this.multiRelations.containsKey(paramId) || this.singleRelations.containsKey(paramId))
			throw new IllegalStateException("Mapping already exists for " + paramId);
	}

	protected void applyParameters(ParameterBagContainer element) {

		this.parametersBags.values().forEach(bagBuilder -> bagBuilder.applyBag(element));

		this.singleRelations.forEach((paramId, keys) -> {
			ParameterBag relationsBag = getRelationsBag(element);
			StringParameter relationP = new StringParameter(paramId, keys[0], "");
			relationP.setInterpretation(getInterpretation(keys[2]));
			relationP.setUom(keys[1]);
			relationsBag.addParameter(relationP);
		});

		this.multiRelations.forEach((paramId, keys) -> {
			ParameterBag relationsBag = getRelationsBag(element);
			StringListParameter relationP = new StringListParameter(paramId, keys[0], emptyList());
			relationP.setInterpretation(getInterpretation(keys[2]));
			relationP.setUom(keys[1]);
			relationsBag.addParameter(relationP);
		});
	}

	protected String getInterpretation(String objectType) {
		return switch (objectType) {
			case Tags.RESOURCE -> INTERPRETATION_RESOURCE_REF;
			case Tags.ORDER -> INTERPRETATION_ORDER_REF;
			case Tags.ACTIVITY -> INTERPRETATION_ACTIVITY_REF;
			default -> throw new IllegalArgumentException("Unexpected object type " + objectType);
		};
	}

	private ParameterBag getRelationsBag(ParameterBagContainer element) {
		ParameterBag relationsBag = element.getParameterBag(BAG_RELATIONS);
		if (relationsBag == null) {
			relationsBag = new ParameterBag(BAG_RELATIONS, TYPE_RELATIONS, TYPE_RELATIONS);
			element.addParameterBag(relationsBag);
		}

		return relationsBag;
	}
}
