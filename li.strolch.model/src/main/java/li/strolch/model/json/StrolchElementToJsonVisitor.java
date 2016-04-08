package li.strolch.model.json;

import java.util.Iterator;
import java.util.Map.Entry;
import java.util.SortedSet;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import ch.eitchnet.utils.iso8601.ISO8601FormatFactory;
import li.strolch.model.AbstractStrolchElement;
import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.policy.PolicyDefs;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.IValue;

public class StrolchElementToJsonVisitor {

	public JsonObject toJson(Resource element) {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.OBJECT_TYPE, Tags.RESOURCE);

		toJson(element, rootJ);

		addParameterBags(element, rootJ);

		addTimedStates(element, rootJ);

		if (element.hasPolicyDefs())
			addPolicies(element.getPolicyDefs(), rootJ);

		return rootJ;
	}

	public JsonObject toJson(Order element) {

		JsonObject rootJ = new JsonObject();
		rootJ.addProperty(Tags.OBJECT_TYPE, Tags.ORDER);

		toJson(element, rootJ);
		rootJ.addProperty(Tags.DATE, ISO8601FormatFactory.getInstance().formatDate(element.getDate()));
		rootJ.addProperty(Tags.STATE, element.getState().name());

		addParameterBags(element, rootJ);

		if (element.hasPolicyDefs())
			addPolicies(element.getPolicyDefs(), rootJ);

		return rootJ;
	}

	public JsonObject toJson(Activity element) {
		JsonObject rootJ = new JsonObject();
		return toJson(element, rootJ);
	}

	public JsonObject toJson(Action element) {
		JsonObject rootJ = new JsonObject();
		return toJson(element, rootJ);
	}

	protected JsonObject toJson(Activity element, JsonObject rootJ) {

		rootJ.addProperty(Tags.OBJECT_TYPE, Tags.ACTIVITY);

		toJson((AbstractStrolchElement) element, rootJ);

		addParameterBags(element, rootJ);

		if (element.hasPolicyDefs())
			addPolicies(element.getPolicyDefs(), rootJ);

		Iterator<Entry<String, IActivityElement>> iter = element.elementIterator();
		if (iter.hasNext()) {

			JsonArray elementsJ = new JsonArray();
			rootJ.add(Tags.ELEMENTS, elementsJ);

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

		rootJ.addProperty(Tags.OBJECT_TYPE, Tags.ACTION);

		toJson((AbstractStrolchElement) element, rootJ);
		rootJ.addProperty(Tags.RESOURCE_ID, element.getResourceId());
		rootJ.addProperty(Tags.RESOURCE_TYPE, element.getResourceType());
		rootJ.addProperty(Tags.STATE, element.getState().name());

		addParameterBags(element, rootJ);

		if (element.hasPolicyDefs())
			addPolicies(element.getPolicyDefs(), rootJ);

		return rootJ;
	}

	protected void addPolicies(PolicyDefs policyDefs, JsonObject rootJ) {
		if (!policyDefs.hasPolicyDefs())
			return;

		JsonObject policyDefsJ = new JsonObject();
		rootJ.add(Tags.POLICIES, policyDefsJ);

		for (String type : policyDefs.getPolicyTypes()) {
			PolicyDef policyDef = policyDefs.getPolicyDef(type);
			policyDefsJ.addProperty(policyDef.getType(), policyDef.getValue());
		}
	}

	protected JsonObject toJson(AbstractStrolchElement element, JsonObject rootJ) {

		rootJ.addProperty(Tags.ID, element.getId());
		rootJ.addProperty(Tags.NAME, element.getName());
		rootJ.addProperty(Tags.TYPE, element.getType());

		return rootJ;
	}

	protected void addParameterBags(GroupedParameterizedElement element, JsonObject rootJ) {

		if (!element.hasParameterBags())
			return;

		JsonObject parameterBagsJ = new JsonObject();
		rootJ.add(Tags.PARAMETER_BAGS, parameterBagsJ);

		for (String bagKey : element.getParameterBagKeySet()) {
			ParameterBag bag = element.getParameterBag(bagKey);

			JsonObject bagJ = new JsonObject();
			parameterBagsJ.add(bagKey, bagJ);

			toJson(bag, bagJ);

			addParameters(bag, bagJ);
		}
	}

	protected void addParameters(ParameterBag bag, JsonObject bagJ) {

		if (!bag.hasParameters())
			return;

		JsonObject paramsJ = new JsonObject();
		bagJ.add(Tags.PARAMETERS, paramsJ);

		for (String paramKey : bag.getParameterKeySet()) {
			Parameter<?> param = bag.getParameter(paramKey);

			JsonObject paramJ = new JsonObject();
			paramsJ.add(paramKey, paramJ);

			toJson((AbstractStrolchElement) param, paramJ);

			paramJ.addProperty(Tags.VALUE, param.getValueAsString());
		}
	}

	protected void addTimedStates(Resource element, JsonObject rootJ) {

		if (!element.hasTimedStates())
			return;

		JsonObject timedStatesJ = new JsonObject();
		rootJ.add(Tags.TIMED_STATES, timedStatesJ);

		for (String stateKey : element.getTimedStateKeySet()) {
			StrolchTimedState<IValue<?>> state = element.getTimedState(stateKey);

			JsonObject stateJ = new JsonObject();
			timedStatesJ.add(stateKey, stateJ);

			toJson((AbstractStrolchElement) state, stateJ);

			JsonArray valuesJ = new JsonArray();
			stateJ.add(Tags.VALUES, valuesJ);

			SortedSet<ITimeValue<IValue<?>>> values = state.getTimeEvolution().getValues();
			for (ITimeValue<IValue<?>> value : values) {

				JsonObject valueJ = new JsonObject();
				valuesJ.add(valueJ);

				Long time = value.getTime();
				String valueS = value.getValue().getValueAsString();

				valueJ.addProperty(Tags.TIME, ISO8601FormatFactory.getInstance().formatDate(time));
				valueJ.addProperty(Tags.VALUE, valueS);
			}
		}
	}
}
