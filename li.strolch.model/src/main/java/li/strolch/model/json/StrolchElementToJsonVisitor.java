package li.strolch.model.json;

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
import li.strolch.model.Tags.Json;
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
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class StrolchElementToJsonVisitor implements StrolchElementVisitor<JsonElement> {

	private MapOfSets<String, String> ignoredKeys;
	private Set<String> ignoredTimedStates;
	private Set<String> ignoredBagTypes;

	private BiConsumer<Resource, JsonObject> resourceHook;
	private BiConsumer<Order, JsonObject> orderHook;
	private BiConsumer<Activity, JsonObject> activityHook;
	private BiConsumer<Action, JsonObject> actionHook;

	private boolean flat;
	private boolean withoutElementName;
	private boolean withVersion;
	private boolean withoutPolicies;
	private int activityDepth = Integer.MAX_VALUE;

	public StrolchElementToJsonVisitor() {
		this.ignoredKeys = new MapOfSets<>();
		this.ignoredTimedStates = new HashSet<>();
		this.ignoredBagTypes = new HashSet<>();
	}

	public boolean isFlat() {
		return this.flat;
	}

	public boolean isWithVersion() {
		return this.withVersion;
	}

	public boolean isWithoutElementName() {
		return this.withoutElementName;
	}

	public boolean isWithoutPolicies() {
		return this.withoutPolicies;
	}

	public StrolchElementToJsonVisitor withVersion() {
		this.withVersion = true;
		return this;
	}

	public StrolchElementToJsonVisitor withoutElementName() {
		this.withoutElementName = true;
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

	public StrolchElementToJsonVisitor activityDepth(int depth) {
		this.activityDepth = depth;
		return this;
	}

	public StrolchElementToJsonVisitor flat() {
		this.flat = true;
		return this;
	}

	public StrolchElementToJsonVisitor ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
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

	public StrolchElementToJsonVisitor ignoreBagByType(String type) {
		this.ignoredBagTypes.add(type);
		return this;
	}

	public StrolchElementToJsonVisitor resourceHook(BiConsumer<Resource, JsonObject> hook) {
		this.resourceHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor orderHook(BiConsumer<Order, JsonObject> hook) {
		this.orderHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor activityHook(BiConsumer<Activity, JsonObject> hook) {
		this.activityHook = hook;
		return this;
	}

	public StrolchElementToJsonVisitor actionHook(BiConsumer<Action, JsonObject> hook) {
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
			return new JsonPrimitive(param.getValueAsString());
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
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitIntegerParam(IntegerParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitLongParam(LongParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
		return paramToJsonFull(param);
	}

	@Override
	public JsonElement visitStringParam(StringParameter param) {
		if (isFlat())
			return new JsonPrimitive(param.getValueAsString());
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
	public JsonElement visitIntegerState(IntegerTimedState state) {
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

		if (isFlat()) {

			JsonObject rootJ = new JsonObject();

			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bag.getId());
			addParameterBagFlat(rootJ, ignoredParamIds, bag);

			return rootJ;
		}

		return parameterBagToJsonFull(bag);
	}

	protected JsonObject toJson(Resource element) {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Json.OBJECT_TYPE, Json.RESOURCE);

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
		rootJ.addProperty(Json.OBJECT_TYPE, Json.ORDER);

		toJson(element, rootJ);
		rootJ.addProperty(Json.DATE, formatDate(element.getDate()));
		rootJ.addProperty(Json.STATE, element.getState().getName());

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

		rootJ.addProperty(Json.OBJECT_TYPE, Json.ACTIVITY);

		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(Json.TIME_ORDERING, element.getTimeOrdering().getName());
		rootJ.addProperty(Json.STATE, element.getState().getName());
		rootJ.addProperty(Json.START, formatDate(element.getStart()));
		rootJ.addProperty(Json.END, formatDate(element.getEnd()));

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
			rootJ.add(Json.ELEMENTS, elementsJ);

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

		rootJ.addProperty(Json.OBJECT_TYPE, Json.ACTION);

		// attributes
		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(Json.RESOURCE_ID, element.getResourceId());
		rootJ.addProperty(Json.RESOURCE_TYPE, element.getResourceType());
		rootJ.addProperty(Json.STATE, element.getState().getName());
		rootJ.addProperty(Json.START, formatDate(element.getStart()));
		rootJ.addProperty(Json.END, formatDate(element.getEnd()));

		addParameterizedElements(element, rootJ);
		addPolicies(element, rootJ);

		if (this.actionHook != null)
			this.actionHook.accept(element, rootJ);

		// value changes
		Iterator<IValueChange<? extends IValue<?>>> iter = element.getChanges().iterator();
		if (iter.hasNext()) {

			JsonArray changesJ = new JsonArray();
			rootJ.add(Json.VALUE_CHANGES, changesJ);

			while (iter.hasNext()) {
				IValueChange<? extends IValue<?>> valueChange = iter.next();

				JsonObject changeJ = new JsonObject();

				if (isNotEmpty(valueChange.getStateId()))
					changeJ.addProperty(Json.STATE_ID, valueChange.getStateId());
				changeJ.addProperty(Json.TIME, formatDate(valueChange.getTime()));
				changeJ.addProperty(Json.VALUE, valueChange.getValue().getValueAsString());
				changeJ.addProperty(Json.TYPE, valueChange.getValue().getType());

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
		rootJ.add(Json.POLICIES, policyDefsJ);

		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);
			policyDefsJ.addProperty(policyDef.getType(), policyDef.getValueForXml());
		}
	}

	protected JsonObject toJson(AbstractStrolchElement element, JsonObject rootJ) {

		rootJ.addProperty(Json.ID, element.getId());

		if (!isWithoutElementName())
			rootJ.addProperty(Json.NAME, element.getName());

		rootJ.addProperty(Json.TYPE, element.getType());

		return rootJ;
	}

	protected void addParameterizedElements(GroupedParameterizedElement element, JsonObject rootJ) {
		if (isFlat())
			addParameterizedElementsFlat(element, rootJ);
		else
			addParameterizedElementsFull(element, rootJ);
	}

	protected void addParameterizedElementsFlat(GroupedParameterizedElement element, JsonObject rootJ) {

		Set<String> bagKeySet = element.getParameterBagKeySet();
		for (String bagId : bagKeySet) {

			// see if we have to ignore this bag i.e. empty set existing
			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bagId);
			if (ignoredParamIds != null && ignoredParamIds.isEmpty())
				continue;

			ParameterBag parameterBag = element.getParameterBag(bagId);
			if (ignoredBagTypes.contains(parameterBag.getType()))
				continue;

			addParameterBagFlat(rootJ, ignoredParamIds, parameterBag);
		}
	}

	private void addParameterBagFlat(JsonObject rootJ, Set<String> ignoredParamIds, ParameterBag parameterBag) {

		Set<String> parameterKeySet = parameterBag.getParameterKeySet();
		for (String paramId : parameterKeySet) {

			// see if this parameter must be ignored
			if (ignoredParamIds != null && ignoredParamIds.contains(paramId))
				continue;

			if (rootJ.has(paramId)) {
				throw new StrolchModelException(
						"JsonObject already has a member with ID " + paramId + ": " + parameterBag.getLocator());
			}

			Parameter<?> param = parameterBag.getParameter(paramId);

			StrolchValueType type = StrolchValueType.parse(param.getType());
			if (type.isBoolean()) {
				rootJ.addProperty(paramId, (Boolean) param.getValue());
			} else if (type.isNumber()) {
				rootJ.addProperty(paramId, (Number) param.getValue());
			} else {
				rootJ.addProperty(paramId, param.getValueAsString());
			}
		}
	}

	protected void addParameterizedElementsFull(GroupedParameterizedElement element, JsonObject rootJ) {

		if (!element.hasParameterBags())
			return;

		JsonObject parameterBagsJ = new JsonObject();
		rootJ.add(Json.PARAMETER_BAGS, parameterBagsJ);

		for (String bagKey : element.getParameterBagKeySet()) {
			ParameterBag bag = element.getParameterBag(bagKey);

			if (!bag.hasParameters())
				continue;

			parameterBagsJ.add(bagKey, parameterBagToJsonFull(bag));
		}
	}

	private JsonObject parameterBagToJsonFull(ParameterBag bag) {
		JsonObject bagJ = new JsonObject();

		toJson(bag, bagJ);

		JsonObject paramsJ = new JsonObject();
		bagJ.add(Json.PARAMETERS, paramsJ);

		for (String paramKey : bag.getParameterKeySet()) {
			Parameter<?> param = bag.getParameter(paramKey);
			paramsJ.add(paramKey, paramToJsonFull(param));
		}

		return bagJ;
	}

	private JsonObject paramToJsonFull(Parameter<?> param) {
		JsonObject paramJ = new JsonObject();

		toJson((AbstractStrolchElement) param, paramJ);

		if (!StrolchModelConstants.INTERPRETATION_NONE.equals(param.getInterpretation()))
			paramJ.addProperty(Json.INTERPRETATION, param.getInterpretation());

		if (param.isHidden())
			paramJ.addProperty(Json.HIDDEN, param.isHidden());

		if (!StrolchModelConstants.UOM_NONE.equals(param.getUom()))
			paramJ.addProperty(Json.UOM, param.getUom());

		if (param.getIndex() != 0)
			paramJ.addProperty(Json.INDEX, param.getIndex());

		paramJ.addProperty(Json.VALUE, param.getValueAsString());
		return paramJ;
	}

	protected void addStates(Resource resource, JsonObject rootJ) {
		if (isFlat())
			addTimedStatesFlat(resource, rootJ);
		else
			addTimedStatesFull(resource, rootJ);
	}

	protected void addTimedStatesFull(Resource element, JsonObject rootJ) {

		if (!element.hasTimedStates())
			return;

		JsonObject timedStatesJ = new JsonObject();
		rootJ.add(Json.TIMED_STATES, timedStatesJ);

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
		stateJ.add(Json.VALUES, valuesJ);

		SortedSet<? extends ITimeValue<? extends IValue<?>>> values = state.getTimeEvolution().getValues();
		for (ITimeValue<? extends IValue<?>> value : values) {

			JsonObject valueJ = new JsonObject();
			valuesJ.add(valueJ);

			Long time = value.getTime();
			String valueS = value.getValue().getValueAsString();

			valueJ.addProperty(Json.TIME, formatDate(time));
			valueJ.addProperty(Json.VALUE, valueS);
		}
		return stateJ;
	}

	private JsonArray stateToJsonFlat(StrolchTimedState<? extends IValue<?>> state) {

		ITimeVariable<? extends IValue<?>> timeEvolution = state.getTimeEvolution();

		JsonArray arrayJ = new JsonArray();
		for (ITimeValue<? extends IValue<?>> v : timeEvolution.getValues()) {
			JsonObject obj = new JsonObject();
			obj.addProperty(Json.DATE, ISO8601FormatFactory.getInstance().formatDate(v.getTime()));
			obj.addProperty(Json.VALUE, v.getValue().getValueAsString());

			arrayJ.add(obj);
		}
		return arrayJ;
	}

	protected void addVersion(StrolchRootElement element, JsonObject rootJ) {
		if (!element.hasVersion())
			return;
		if (!isWithVersion())
			return;

		Version version = element.getVersion();

		JsonObject versionJ = new JsonObject();
		versionJ.addProperty(Json.VERSION, version.getVersion());
		versionJ.addProperty(Json.CREATED_BY, version.getCreatedBy());
		versionJ.addProperty(Json.CREATED_AT, formatDate(version.getCreatedAt()));
		versionJ.addProperty(Json.DELETED, version.isDeleted());
		rootJ.add(Json.VERSION, versionJ);
	}

	private static String formatDate(Date date) {
		return ISO8601FormatFactory.getInstance().formatDate(date);
	}

	private static String formatDate(Long timestamp) {
		return ISO8601FormatFactory.getInstance().formatDate(timestamp);
	}
}
