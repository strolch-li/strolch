package li.strolch.model.json;

import static java.util.Arrays.asList;
import static java.util.Comparator.comparing;
import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;
import static li.strolch.model.Tags.Json.*;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.*;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.*;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.*;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class StrolchElementToJsonVisitor implements StrolchElementVisitor<JsonElement> {

	private final MapOfSets<String, String> ignoredKeys;
	private final Set<String> ignoredTimedStates;
	private final Set<String> ignoredBagTypes;

	private BiConsumer<ParameterBag, JsonObject> bagHook;
	private BiConsumer<Resource, JsonObject> resourceHook;
	private BiConsumer<Order, JsonObject> orderHook;
	private BiConsumer<Activity, JsonObject> activityHook;
	private BiConsumer<Action, JsonObject> actionHook;

	private boolean flat;
	private final Set<String> flatBags;
	private final Set<String> flatBagsByType;
	private boolean withoutElementName;
	private boolean withoutObjectType;
	private boolean withLocator;
	private boolean withBagId;
	private boolean withoutVersion;
	private boolean withoutPolicies;
	private boolean withoutStateVariables;
	private boolean withoutValueChanges;
	private boolean withListParametersAsArray;
	private int activityDepth = Integer.MAX_VALUE;

	public StrolchElementToJsonVisitor() {
		this.ignoredKeys = new MapOfSets<>();
		this.ignoredTimedStates = new HashSet<>();
		this.ignoredBagTypes = new HashSet<>();
		this.flatBags = new HashSet<>();
		this.flatBagsByType = new HashSet<>();
	}

	public boolean isFlat() {
		return this.flat;
	}

	public boolean isBagFlat(ParameterBag bag) {
		return this.flatBags.contains(bag.getId()) || this.flatBagsByType.contains(bag.getType());
	}

	public boolean isWithVersion() {
		return !this.withoutVersion;
	}

	public boolean isWithLocator() {
		return !this.withLocator;
	}

	public boolean isWithoutVersion() {
		return this.withoutVersion;
	}

	public boolean isWithoutElementName() {
		return this.withoutElementName;
	}

	public boolean isWithoutPolicies() {
		return this.withoutPolicies;
	}

	public boolean isWithoutStateVariables() {
		return this.withoutStateVariables;
	}

	public boolean isWithoutValueChanges() {
		return this.withoutValueChanges;
	}

	public boolean isWithListParametersAsArray() {
		return this.withListParametersAsArray;
	}

	public StrolchElementToJsonVisitor withBagId() {
		this.withBagId = true;
		return this;
	}

	public StrolchElementToJsonVisitor withLocator() {
		this.withLocator = true;
		return this;
	}

	public StrolchElementToJsonVisitor withVersion() {
		this.withoutVersion = false;
		return this;
	}

	public StrolchElementToJsonVisitor withoutVersion() {
		this.withoutVersion = true;
		return this;
	}

	public StrolchElementToJsonVisitor withoutElementName() {
		this.withoutElementName = true;
		return this;
	}

	public StrolchElementToJsonVisitor withoutObjectType() {
		this.withoutObjectType = true;
		return this;
	}

	public StrolchElementToJsonVisitor withElementName() {
		this.withoutElementName = false;
		return this;
	}

	public StrolchElementToJsonVisitor withoutPolicies() {
		this.withoutPolicies = true;
		return this;
	}

	public StrolchElementToJsonVisitor withPolicies() {
		this.withoutPolicies = false;
		return this;
	}

	public StrolchElementToJsonVisitor withoutValueChanges() {
		this.withoutValueChanges = true;
		return this;
	}

	public StrolchElementToJsonVisitor withoutStateVariables() {
		this.withoutStateVariables = true;
		return this;
	}

	public StrolchElementToJsonVisitor withStateVariables() {
		this.withoutStateVariables = false;
		return this;
	}

	public StrolchElementToJsonVisitor withListParametersAsArray() {
		this.withListParametersAsArray = true;
		return this;
	}

	public StrolchElementToJsonVisitor activityDepth(int depth) {
		this.activityDepth = depth;
		return this;
	}

	public StrolchElementToJsonVisitor flat() {
		this.flat = true;
		return this;
	}

	public StrolchElementToJsonVisitor flatBags(String... bagIds) {
		this.flatBags.addAll(asList(bagIds));
		return this;
	}

	public StrolchElementToJsonVisitor flatBagsByType(String... bagTypes) {
		this.flatBagsByType.addAll(asList(bagTypes));
		return this;
	}

	public StrolchElementToJsonVisitor ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
		return this;
	}

	public StrolchElementToJsonVisitor ignoreBags(String... bagIds) {
		for (String bagId : bagIds) {
			this.ignoredKeys.addSet(bagId, Collections.emptySet());
		}
		return this;
	}

	public StrolchElementToJsonVisitor ignoreParameter(String bagId, String paramId) {
		this.ignoredKeys.addElement(bagId, paramId);
		return this;
	}

	public StrolchElementToJsonVisitor ignoreTimeState(String timeStateId) {
		this.ignoredTimedStates.add(timeStateId);
		return this;
	}

	public StrolchElementToJsonVisitor ignoreBagByType(String... types) {
		this.ignoredBagTypes.addAll(asList(types));
		return this;
	}

	public StrolchElementToJsonVisitor bagHook(BiConsumer<ParameterBag, JsonObject> hook) {
		this.bagHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor resourceHook(BiConsumer<Resource, JsonObject> hook) {
		DBC.PRE.assertNotNull("hook must not be null", hook);
		DBC.PRE.assertNull("resourceHook already set!", this.resourceHook);
		this.resourceHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor orderHook(BiConsumer<Order, JsonObject> hook) {
		DBC.PRE.assertNotNull("hook must not be null", hook);
		DBC.PRE.assertNull("orderHook already set!", this.orderHook);
		this.orderHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor activityHook(BiConsumer<Activity, JsonObject> hook) {
		DBC.PRE.assertNotNull("hook must not be null", hook);
		DBC.PRE.assertNull("activityHook already set!", this.activityHook);
		this.activityHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor actionHook(BiConsumer<Action, JsonObject> hook) {
		DBC.PRE.assertNotNull("hook must not be null", hook);
		DBC.PRE.assertNull("actionHook already set!", this.actionHook);
		this.actionHook = hook;
		return this;
	}

	@Override
	public JsonElement visitResource(Resource resource) {
		return toJson(resource);
	}

	@Override
	public JsonElement visitOrder(Order order) {
		return toJson(order);
	}

	@Override
	public JsonElement visitActivity(Activity activity) {
		return toJson(activity);
	}

	@Override
	public JsonElement visitAction(Action action) {
		return toJson(action);
	}

	@Override
	public JsonElement visitBooleanParam(BooleanParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitDateParam(DateParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitDurationParam(DurationParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitFloatParam(FloatParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitIntegerParam(IntegerParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitLongParam(LongParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitStringParam(StringParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitTextParam(TextParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValue());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitStringListParam(StringListParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitIntegerListParam(IntegerListParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitFloatListParam(FloatListParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitLongListParam(LongListParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitBooleanState(BooleanTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitFloatState(FloatTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitFloatListState(FloatListTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitIntegerListState(IntegerListTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitIntegerState(IntegerTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitLongState(LongTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonElement visitStringState(StringSetTimedState state) {
		if (isFlat())
			return stateToJsonFlat(state);
		return stateToJsonFull(state);
	}

	@Override
	public JsonObject visitParameterBag(ParameterBag bag) {

		if (isFlat() || isBagFlat(bag)) {

			JsonObject bagJ = new JsonObject();
			if (this.withBagId)
				bagJ.addProperty(ID, bag.getId());

			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bag.getId());
			addParameterBagFlat(bagJ, ignoredParamIds, bag);

			if (this.bagHook != null)
				this.bagHook.accept(bag, bagJ);

			return bagJ;
		}

		return parameterBagToJsonFull(bag);
	}

	protected JsonObject toJson(Resource element) {

		JsonObject rootJ = new JsonObject();
		if (!this.withoutObjectType)
			rootJ.addProperty(OBJECT_TYPE, RESOURCE);

		toJson(element, rootJ);

		addVersion(element, rootJ);
		addParameterizedElements(element, rootJ);
		addStates(element, rootJ);
		addPolicies(element, rootJ);

		if (this.resourceHook != null)
			this.resourceHook.accept(element, rootJ);

		return rootJ;
	}

	protected JsonObject toJson(Order element) {

		JsonObject rootJ = new JsonObject();
		if (!this.withoutObjectType)
			rootJ.addProperty(OBJECT_TYPE, ORDER);

		toJson(element, rootJ);
		rootJ.addProperty(DATE, formatDate(element.getDate()));
		rootJ.addProperty(STATE, element.getState().getName());

		addVersion(element, rootJ);
		addParameterizedElements(element, rootJ);
		addPolicies(element, rootJ);

		if (this.orderHook != null)
			this.orderHook.accept(element, rootJ);

		return rootJ;
	}

	protected JsonObject toJson(Activity element) {
		JsonObject rootJ = new JsonObject();
		return toJson(element, rootJ, 0);
	}

	protected JsonObject toJson(Action element) {
		JsonObject rootJ = new JsonObject();
		return toJson(element, rootJ);
	}

	protected JsonObject toJson(Activity element, JsonObject rootJ, int currentDepth) {

		if (!this.withoutObjectType)
			rootJ.addProperty(OBJECT_TYPE, ACTIVITY);

		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(TIME_ORDERING, element.getTimeOrdering().getName());
		rootJ.addProperty(STATE, element.getState().getName());
		rootJ.addProperty(START, formatDate(element.getStart()));
		rootJ.addProperty(END, formatDate(element.getEnd()));

		if (element.isRootElement())
			addVersion(element, rootJ);
		addParameterizedElements(element, rootJ);
		addPolicies(element, rootJ);

		if (this.activityHook != null)
			this.activityHook.accept(element, rootJ);

		if (currentDepth >= this.activityDepth)
			return rootJ;

		Iterator<Entry<String, IActivityElement>> iter = element.elementIterator();
		if (iter.hasNext()) {

			JsonArray elementsJ = new JsonArray();
			rootJ.add(ELEMENTS, elementsJ);

			while (iter.hasNext()) {
				IActivityElement activityElement = iter.next().getValue();

				JsonObject elementJ = new JsonObject();
				elementsJ.add(elementJ);

				if (activityElement instanceof Activity) {
					toJson((Activity) activityElement, elementJ, currentDepth + 1);
				} else if (activityElement instanceof Action) {
					toJson((Action) activityElement, elementJ);
				} else {
					throw new IllegalArgumentException("Unhandled element " + activityElement.getClass());
				}
			}
		}

		return rootJ;
	}

	protected JsonObject toJson(Action element, JsonObject rootJ) {

		if (!this.withoutObjectType)
			rootJ.addProperty(OBJECT_TYPE, ACTION);

		// attributes
		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(RESOURCE_ID, element.getResourceId());
		rootJ.addProperty(RESOURCE_TYPE, element.getResourceType());
		rootJ.addProperty(STATE, element.getState().getName());
		rootJ.addProperty(START, formatDate(element.getStart()));
		rootJ.addProperty(END, formatDate(element.getEnd()));

		addParameterizedElements(element, rootJ);
		addPolicies(element, rootJ);

		if (this.actionHook != null)
			this.actionHook.accept(element, rootJ);

		// value changes
		if (!this.withoutValueChanges && element.hasChanges()) {
			Iterator<IValueChange<? extends IValue<?>>> iter = element.getChanges().iterator();

			JsonArray changesJ = new JsonArray();
			rootJ.add(VALUE_CHANGES, changesJ);

			while (iter.hasNext()) {
				IValueChange<? extends IValue<?>> valueChange = iter.next();

				JsonObject changeJ = new JsonObject();

				if (isNotEmpty(valueChange.getStateId()))
					changeJ.addProperty(STATE_ID, valueChange.getStateId());
				changeJ.addProperty(TIME, formatDate(valueChange.getTime()));
				changeJ.addProperty(VALUE, valueChange.getValue().getValueAsString());
				changeJ.addProperty(TYPE, valueChange.getValue().getType());

				changesJ.add(changeJ);
			}
		}

		return rootJ;
	}

	protected void addPolicies(PolicyContainer policyContainer, JsonObject rootJ) {
		if (!policyContainer.hasPolicyDefs() || !policyContainer.getPolicyDefs().hasPolicyDefs())
			return;
		if (isWithoutPolicies())
			return;

		PolicyDefs policyDefs = policyContainer.getPolicyDefs();

		JsonObject policyDefsJ = new JsonObject();
		rootJ.add(POLICIES, policyDefsJ);

		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);
			policyDefsJ.addProperty(policyDef.getType(), policyDef.getValueForXml());
		}
	}

	protected JsonObject toJson(AbstractStrolchElement element, JsonObject rootJ) {

		rootJ.addProperty(ID, element.getId());
		if (this.withLocator)
			rootJ.addProperty(LOCATOR, element.getLocator().toString());

		if (!isWithoutElementName())
			rootJ.addProperty(NAME, element.getName());

		rootJ.addProperty(TYPE, element.getType());

		return rootJ;
	}

	protected void addParameterizedElements(GroupedParameterizedElement element, JsonObject rootJ) {

		if (!element.hasParameterBags())
			return;

		for (String bagId : element.getParameterBagKeySet()) {
			ParameterBag bag = element.getParameterBag(bagId);
			if (!bag.hasParameters())
				continue;

			// see if we have to ignore this bag i.e. empty set existing
			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bagId);
			if (ignoredParamIds != null && ignoredParamIds.isEmpty())
				continue;

			ParameterBag parameterBag = element.getParameterBag(bagId);
			if (this.ignoredBagTypes.contains(parameterBag.getType()))
				continue;

			if (isBagFlat(bag)) {

				JsonObject bagJ = new JsonObject();
				addParameterBagFlat(bagJ, ignoredParamIds, parameterBag);
				rootJ.add(bagId, bagJ);

			} else if (isFlat()) {

				addParameterBagFlat(rootJ, ignoredParamIds, parameterBag);

			} else {

				JsonObject parameterBagsJ;
				if (rootJ.has(PARAMETER_BAGS)) {
					parameterBagsJ = rootJ.get(PARAMETER_BAGS).getAsJsonObject();
				} else {
					parameterBagsJ = new JsonObject();
					rootJ.add(PARAMETER_BAGS, parameterBagsJ);
				}

				parameterBagsJ.add(bagId, parameterBagToJsonFull(bag));
			}
		}
	}

	private void addParameterBagFlat(JsonObject rootJ, Set<String> ignoredParamIds, ParameterBag parameterBag) {
		parameterBag.streamOfParameters().sorted(comparing(Parameter::getIndex)).forEach(param -> {
			String paramId = param.getId();

			// see if this parameter must be ignored
			if (ignoredParamIds != null && ignoredParamIds.contains(paramId))
				return;

			if (rootJ.has(paramId)) {
				throw new StrolchModelException(
						"JsonObject already has a member with ID " + paramId + ": " + param.getLocator());
			}

			StrolchValueType type = StrolchValueType.parse(param.getType());
			if (type.isBoolean()) {
				rootJ.addProperty(paramId, (Boolean) param.getValue());
			} else if (type.isNumber()) {
				rootJ.addProperty(paramId, (Number) param.getValue());
			} else if (this.withListParametersAsArray && type.isList()) {
				JsonArray valuesJ = switch (type) {
					case FLOAT_LIST -> ((FloatListParameter) param).streamValues()
							.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
					case INTEGER_LIST -> ((IntegerListParameter) param).streamValues()
							.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
					case STRING_LIST -> ((StringListParameter) param).streamValues()
							.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
					case LONG_LIST -> ((LongListParameter) param).streamValues()
							.collect(JsonArray::new, JsonArray::add, JsonArray::addAll);
					default -> throw new IllegalStateException("Unhandle list type " + type);
				};
				rootJ.add(paramId, valuesJ);
			} else {
				rootJ.addProperty(paramId, param.getValueAsString());
			}
		});
	}

	private JsonObject parameterBagToJsonFull(ParameterBag bag) {
		JsonObject bagJ = new JsonObject();

		toJson(bag, bagJ);

		JsonObject paramsJ = new JsonObject();
		bagJ.add(PARAMETERS, paramsJ);

		bag.streamOfParameters()
				.sorted(comparing(Parameter::getIndex))
				.forEach(param -> paramsJ.add(param.getId(), paramToJsonFull(param)));

		if (this.bagHook != null)
			this.bagHook.accept(bag, bagJ);

		return bagJ;
	}

	private JsonObject paramToJsonFull(Parameter<?> param) {
		JsonObject paramJ = new JsonObject();

		toJson((AbstractStrolchElement) param, paramJ);

		if (!INTERPRETATION_NONE.equals(param.getInterpretation()))
			paramJ.addProperty(INTERPRETATION, param.getInterpretation());

		if (param.isHidden())
			paramJ.addProperty(HIDDEN, param.isHidden());

		if (!UOM_NONE.equals(param.getUom()))
			paramJ.addProperty(UOM, param.getUom());

		if (param.getIndex() != 0)
			paramJ.addProperty(INDEX, param.getIndex());

		paramJ.addProperty(VALUE, param.getValueAsString());
		return paramJ;
	}

	protected void addStates(Resource resource, JsonObject rootJ) {
		if (isWithoutStateVariables())
			return;

		if (isFlat())
			addTimedStatesFlat(resource, rootJ);
		else
			addTimedStatesFull(resource, rootJ);
	}

	protected void addTimedStatesFull(Resource element, JsonObject rootJ) {

		if (!element.hasTimedStates())
			return;

		JsonObject timedStatesJ = new JsonObject();
		rootJ.add(TIMED_STATES, timedStatesJ);

		for (String stateKey : element.getTimedStateKeySet()) {

			// see if we have to ignore this state
			if (this.ignoredTimedStates.contains(stateKey))
				continue;

			StrolchTimedState<IValue<?>> state = element.getTimedState(stateKey);
			timedStatesJ.add(stateKey, stateToJsonFull(state));
		}
	}

	private void addTimedStatesFlat(Resource element, JsonObject rootJ) {

		if (!element.hasTimedStates())
			return;

		for (String stateKey : element.getTimedStateKeySet()) {

			// see if we have to ignore this state
			if (this.ignoredTimedStates.contains(stateKey))
				continue;

			StrolchTimedState<IValue<?>> state = element.getTimedState(stateKey);

			if (rootJ.has(stateKey)) {
				throw new StrolchModelException(
						"JsonObject already has a member with ID " + stateKey + ": " + state.getLocator());
			}

			// build JSON data
			rootJ.add(stateKey, stateToJsonFlat(state));
		}
	}

	private JsonObject stateToJsonFull(StrolchTimedState<? extends IValue<?>> state) {
		JsonObject stateJ = new JsonObject();

		toJson((AbstractStrolchElement) state, stateJ);

		JsonArray valuesJ = new JsonArray();
		stateJ.add(VALUES, valuesJ);

		SortedSet<? extends ITimeValue<? extends IValue<?>>> values = state.getTimeEvolution().getValues();
		for (ITimeValue<? extends IValue<?>> value : values) {

			JsonObject valueJ = new JsonObject();
			valuesJ.add(valueJ);

			Long time = value.getTime();
			String valueS = value.getValue().getValueAsString();

			valueJ.addProperty(TIME, formatDate(time));
			valueJ.addProperty(VALUE, valueS);
		}
		return stateJ;
	}

	private JsonArray stateToJsonFlat(StrolchTimedState<? extends IValue<?>> state) {

		ITimeVariable<? extends IValue<?>> timeEvolution = state.getTimeEvolution();

		JsonArray arrayJ = new JsonArray();
		for (ITimeValue<? extends IValue<?>> v : timeEvolution.getValues()) {
			JsonObject obj = new JsonObject();
			obj.addProperty(DATE, ISO8601FormatFactory.getInstance().formatDate(v.getTime()));
			obj.addProperty(VALUE, v.getValue().getValueAsString());

			arrayJ.add(obj);
		}
		return arrayJ;
	}

	protected void addVersion(StrolchRootElement element, JsonObject rootJ) {
		if (!element.hasVersion())
			return;
		if (isWithoutVersion())
			return;

		Version version = element.getVersion();

		JsonObject versionJ = new JsonObject();
		versionJ.addProperty(VERSION, version.getVersion());
		versionJ.addProperty(CREATED_BY, version.getCreatedBy());
		versionJ.addProperty(UPDATED_BY, version.getUpdatedBy());
		versionJ.addProperty(CREATED, formatDate(version.getCreated()));
		versionJ.addProperty(UPDATED, formatDate(version.getUpdated()));
		versionJ.addProperty(DELETED, version.isDeleted());
		rootJ.add(VERSION, versionJ);
	}

	private static String formatDate(Date date) {
		return ISO8601FormatFactory.getInstance().formatDate(date);
	}

	private static String formatDate(Long timestamp) {
		return ISO8601FormatFactory.getInstance().formatDate(timestamp);
	}
}
