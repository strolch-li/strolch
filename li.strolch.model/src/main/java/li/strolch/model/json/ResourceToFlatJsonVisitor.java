package li.strolch.model.json;

import java.util.HashSet;
import java.util.Set;
import java.util.function.BiConsumer;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.Resource;
import li.strolch.model.Tags;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.visitor.ResourceVisitor;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class ResourceToFlatJsonVisitor extends ToFlatJsonVisitor<Resource> implements ResourceVisitor<JsonObject> {

	private Set<String> ignoredStates;
	private boolean withoutTimedStates;

	public ResourceToFlatJsonVisitor() {
		super();
		this.ignoredStates = new HashSet<>();
	}

	@Override
	public ResourceToFlatJsonVisitor withVersion() {
		super.withVersion();
		return this;
	}

	public ResourceToFlatJsonVisitor withoutTimedStates() {
		this.withoutTimedStates = true;
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor withoutElementName() {
		super.withoutElementName();
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor hook(BiConsumer<Resource, JsonObject> hook) {
		super.hook(hook);
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor ignoreBag(String bagId) {
		super.ignoreBag(bagId);
		return this;
	}

	@Override
	public ResourceToFlatJsonVisitor ignoreParameter(String bagId, String paramId) {
		super.ignoreParameter(bagId, paramId);
		return this;
	}

	@Override
	public JsonObject visit(Resource element) {
		if (this.withoutTimedStates || !element.hasTimedStates())
			return toJson(element);

		JsonObject jsonObject = toJson(element);
		addStates(element, jsonObject);
		return jsonObject;
	}

	private void addStates(Resource element, JsonObject jsonObject) {
		for (String stateKey : element.getTimedStateKeySet()) {

			// see if we have to ignore this state
			if (this.ignoredStates.contains(stateKey))
				continue;

			// get values
			StrolchTimedState<IValue<?>> stateT = element.getTimedState(stateKey);
			ITimeVariable<IValue<?>> timeEvolution = stateT.getTimeEvolution();

			if (jsonObject.has(stateKey)) {
				throw new StrolchModelException(
						"JsonObject already has a member with ID " + stateKey + ": " + stateT.getLocator());
			}

			// build JSON data
			JsonArray arrayJ = new JsonArray();
			for (ITimeValue<IValue<?>> v : timeEvolution.getValues()) {
				JsonObject obj = new JsonObject();
				obj.addProperty(Tags.Json.DATE, ISO8601FormatFactory.getInstance().formatDate(v.getTime()));
				obj.addProperty(Tags.Json.VALUE, v.getValue().getValueAsString());

				arrayJ.add(obj);
			}

			jsonObject.add(stateKey, arrayJ);
		}
	}
}
