package li.strolch.model.json;

import java.util.Date;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.SortedSet;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.PolicyContainer;
import li.strolch.model.Resource;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.Version;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class StrolchElementToJsonVisitor implements StrolchRootElementVisitor<JsonObject> {

	@Override
	public JsonObject visitResource(Resource resource) {
		return toJson(resource);
	}

	@Override
	public JsonObject visitOrder(Order order) {
		return toJson(order);
	}

	@Override
	public JsonObject visitActivity(Activity activity) {
		return toJson(activity);
	}

	protected JsonObject toJson(Resource element) {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.RESOURCE);

		toJson(element, rootJ);

		addVersion(element, rootJ);
		addParameterBags(element, rootJ);
		addTimedStates(element, rootJ);
		addPolicies(element, rootJ);

		return rootJ;
	}

	protected JsonObject toJson(Order element) {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ORDER);

		toJson(element, rootJ);
		rootJ.addProperty(Tags.Json.DATE, formatDate(element.getDate()));
		rootJ.addProperty(Tags.Json.STATE, element.getState().getName());

		addVersion(element, rootJ);
		addParameterBags(element, rootJ);
		addPolicies(element, rootJ);

		return rootJ;
	}

	protected JsonObject toJson(Activity element) {
		JsonObject rootJ = new JsonObject();
		addVersion(element, rootJ);
		return toJson(element, rootJ);
	}

	protected JsonObject toJson(Action element) {
		JsonObject rootJ = new JsonObject();
		return toJson(element, rootJ);
	}

	protected JsonObject toJson(Activity element, JsonObject rootJ) {

		rootJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTIVITY);

		rootJ.addProperty(Tags.Json.TIME_ORDERING, element.getTimeOrdering().getName());
		rootJ.addProperty(Tags.Json.STATE, element.getState().getName());
		rootJ.addProperty(Tags.Json.START, formatDate(element.getStart()));
		rootJ.addProperty(Tags.Json.END, formatDate(element.getEnd()));

		toJson((AbstractStrolchElement) element, rootJ);

		addParameterBags(element, rootJ);
		addPolicies(element, rootJ);

		Iterator<Entry<String, IActivityElement>> iter = element.elementIterator();
		if (iter.hasNext()) {

			JsonArray elementsJ = new JsonArray();
			rootJ.add(Tags.Json.ELEMENTS, elementsJ);

			while (iter.hasNext()) {
				IActivityElement activityElement = iter.next().getValue();

				JsonObject elementJ = new JsonObject();
				elementsJ.add(elementJ);

				if (activityElement instanceof Activity) {
					toJson((Activity) activityElement, elementJ);
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

		rootJ.addProperty(Tags.Json.OBJECT_TYPE, Tags.Json.ACTION);

		// attributes
		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(Tags.Json.RESOURCE_ID, element.getResourceId());
		rootJ.addProperty(Tags.Json.RESOURCE_TYPE, element.getResourceType());
		rootJ.addProperty(Tags.Json.STATE, element.getState().getName());
		rootJ.addProperty(Tags.Json.START, formatDate(element.getStart()));
		rootJ.addProperty(Tags.Json.END, formatDate(element.getEnd()));

		addParameterBags(element, rootJ);
		addPolicies(element, rootJ);

		// value changes
		Iterator<IValueChange<? extends IValue<?>>> iter = element.getChanges().iterator();
		if (iter.hasNext()) {

			JsonArray changesJ = new JsonArray();
			rootJ.add(Tags.Json.VALUE_CHANGES, changesJ);

			while (iter.hasNext()) {
				IValueChange<? extends IValue<?>> valueChange = iter.next();

				JsonObject changeJ = new JsonObject();

				changeJ.addProperty(Tags.Json.STATE_ID, valueChange.getStateId());
				changeJ.addProperty(Tags.Json.TIME, formatDate(valueChange.getTime()));
				changeJ.addProperty(Tags.Json.VALUE, valueChange.getValue().getValueAsString());
				changeJ.addProperty(Tags.Json.TYPE, valueChange.getValue().getType());

				changesJ.add(changeJ);
			}
		}

		return rootJ;
	}

	protected void addPolicies(PolicyContainer policyContainer, JsonObject rootJ) {
		if (!policyContainer.hasPolicyDefs() || !policyContainer.getPolicyDefs().hasPolicyDefs())
			return;

		PolicyDefs policyDefs = policyContainer.getPolicyDefs();

		JsonObject policyDefsJ = new JsonObject();
		rootJ.add(Tags.Json.POLICIES, policyDefsJ);

		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);
			policyDefsJ.addProperty(policyDef.getType(), policyDef.getValueForXml());
		}
	}

	protected JsonObject toJson(AbstractStrolchElement element, JsonObject rootJ) {

		rootJ.addProperty(Tags.Json.ID, element.getId());
		rootJ.addProperty(Tags.Json.NAME, element.getName());
		rootJ.addProperty(Tags.Json.TYPE, element.getType());

		return rootJ;
	}

	protected void addParameterBags(GroupedParameterizedElement element, JsonObject rootJ) {

		if (!element.hasParameterBags())
			return;

		JsonObject parameterBagsJ = new JsonObject();
		rootJ.add(Tags.Json.PARAMETER_BAGS, parameterBagsJ);

		for (String bagKey : element.getParameterBagKeySet()) {
			ParameterBag bag = element.getParameterBag(bagKey);

			JsonObject bagJ = new JsonObject();
			parameterBagsJ.add(bagKey, bagJ);

			toJson(bag, bagJ);

			if (!bag.hasParameters())
				continue;

			JsonObject paramsJ = new JsonObject();
			bagJ.add(Tags.Json.PARAMETERS, paramsJ);

			for (String paramKey : bag.getParameterKeySet()) {
				Parameter<?> param = bag.getParameter(paramKey);

				JsonObject paramJ = new JsonObject();
				paramsJ.add(paramKey, paramJ);

				toJson((AbstractStrolchElement) param, paramJ);

				if (!StrolchModelConstants.INTERPRETATION_NONE.equals(param.getInterpretation()))
					paramJ.addProperty(Tags.Json.INTERPRETATION, param.getInterpretation());

				if (param.isHidden())
					paramJ.addProperty(Tags.Json.HIDDEN, param.isHidden());

				if (!StrolchModelConstants.UOM_NONE.equals(param.getUom()))
					paramJ.addProperty(Tags.Json.UOM, param.getUom());

				if (param.getIndex() != 0)
					paramJ.addProperty(Tags.Json.INDEX, param.getIndex());

				paramJ.addProperty(Tags.Json.VALUE, param.getValueAsString());
			}
		}
	}

	protected void addTimedStates(Resource element, JsonObject rootJ) {

		if (!element.hasTimedStates())
			return;

		JsonObject timedStatesJ = new JsonObject();
		rootJ.add(Tags.Json.TIMED_STATES, timedStatesJ);

		for (String stateKey : element.getTimedStateKeySet()) {
			StrolchTimedState<IValue<?>> state = element.getTimedState(stateKey);

			JsonObject stateJ = new JsonObject();
			timedStatesJ.add(stateKey, stateJ);

			toJson((AbstractStrolchElement) state, stateJ);

			JsonArray valuesJ = new JsonArray();
			stateJ.add(Tags.Json.VALUES, valuesJ);

			SortedSet<ITimeValue<IValue<?>>> values = state.getTimeEvolution().getValues();
			for (ITimeValue<IValue<?>> value : values) {

				JsonObject valueJ = new JsonObject();
				valuesJ.add(valueJ);

				Long time = value.getTime();
				String valueS = value.getValue().getValueAsString();

				valueJ.addProperty(Tags.Json.TIME, formatDate(time));
				valueJ.addProperty(Tags.Json.VALUE, valueS);
			}
		}
	}

	public static void addVersion(StrolchRootElement element, JsonObject rootJ) {
		if (!element.hasVersion())
			return;

		Version version = element.getVersion();

		JsonObject versionJ = new JsonObject();
		versionJ.addProperty(Tags.Json.VERSION, version.getVersion());
		versionJ.addProperty(Tags.Json.CREATED_BY, version.getCreatedBy());
		versionJ.addProperty(Tags.Json.CREATED_AT, formatDate(version.getCreatedAt()));
		versionJ.addProperty(Tags.Json.DELETED, version.isDeleted());
		rootJ.add(Tags.Json.VERSION, versionJ);
	}
	

	private static String formatDate(Date date) {
		return ISO8601FormatFactory.getInstance().formatDate(date);
	}

	private static String formatDate(Long timestamp) {
		return ISO8601FormatFactory.getInstance().formatDate(timestamp);
	}
}
