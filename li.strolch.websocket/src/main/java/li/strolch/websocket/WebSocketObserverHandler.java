package li.strolch.websocket;

import static li.strolch.model.Tags.Json.*;
import static li.strolch.rest.StrolchRestfulConstants.DATA;
import static li.strolch.rest.StrolchRestfulConstants.MSG;
import static li.strolch.utils.helper.StringHelper.DASH;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import li.strolch.agent.api.Observer;
import li.strolch.agent.api.ObserverHandler;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags;
import li.strolch.model.json.StrolchElementToJsonVisitor;
import li.strolch.utils.collections.MapOfLists;
import li.strolch.utils.collections.MapOfSets;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WebSocketObserverHandler implements Observer {

	protected static final Logger logger = LoggerFactory.getLogger(WebSocketObserverHandler.class);

	protected ObserverHandler observerHandler;
	protected WebSocketClient client;

	protected MapOfSets<String, String> observedTypes;
	protected Map<String, JsonObject> params;

	public WebSocketObserverHandler(ObserverHandler observerHandler, WebSocketClient client) {
		this.observerHandler = observerHandler;
		this.client = client;
		this.observedTypes = new MapOfSets<>();
		this.params = new HashMap<>(1);
	}

	public void register(String objectType, String type, JsonObject params) {
		if (!this.observedTypes.containsSet(objectType))
			this.observerHandler.registerObserver(objectType, this);
		this.observedTypes.addElement(objectType, type);
		this.params.put(type, params);
	}

	public void unregister(String objectType, String type) {
		this.observedTypes.removeElement(objectType, type);
		if (!this.observedTypes.containsSet(objectType))
			this.observerHandler.unregisterObserver(objectType, this);
	}

	public void unregisterAll() {
		this.observedTypes.keySet().forEach(key -> this.observerHandler.unregisterObserver(key, this));
	}

	@Override
	public void add(String key, List<StrolchRootElement> elements) {
		handleUpdate("ObserverAdd", key, elements);
	}

	@Override
	public void update(String key, List<StrolchRootElement> elements) {
		handleUpdate("ObserverUpdate", key, elements);
	}

	@Override
	public void remove(String key, List<StrolchRootElement> elements) {
		handleUpdate("ObserverRemove", key, elements);
	}

	protected void handleUpdate(String updateType, String key, List<StrolchRootElement> elements) {
		Set<String> observedTypesSet = this.observedTypes.getSet(key);
		if (observedTypesSet == null)
			return;

		MapOfLists<String, JsonObject> data = elements //
				.stream() //
				.filter(e -> filter(observedTypesSet, e)) //
				.map(this::toJson) //
				.collect(MapOfLists::new, //
						(mol, e) -> mol.addElement(e.get(Tags.Json.TYPE).getAsString(), e), //
						MapOfLists::addAll);
		if (data.isEmpty())
			return;

		data.keySet().forEach(type -> {
			JsonObject jsonObject = new JsonObject();
			jsonObject.addProperty(MSG, DASH);
			jsonObject.addProperty(Tags.Json.MSG_TYPE, updateType);
			jsonObject.addProperty(Tags.Json.OBJECT_TYPE, key);
			jsonObject.addProperty(Tags.Json.TYPE, type);
			JsonArray elementsJ = new JsonArray();
			data.getList(type).forEach(elementsJ::add);
			jsonObject.add(DATA, elementsJ);
			this.client.sendMessage(jsonObject.toString());
		});
	}

	protected boolean filter(Set<String> observedTypesSet, StrolchRootElement e) {
		return observedTypesSet.contains("*") || observedTypesSet.contains(e.getType());
	}

	protected JsonObject toJson(StrolchRootElement e) {

		StrolchElementToJsonVisitor visitor = new StrolchElementToJsonVisitor();

		JsonObject params = this.params.get(e.getType());
		if (params == null)
			params = this.params.get("*");
		if (params != null) {

			if (params.has(FLAT) && params.get(FLAT).getAsBoolean())
				visitor.flat();
			if (params.has(WITH_LOCATOR) && params.get(WITH_LOCATOR).getAsBoolean())
				visitor.withLocator();
			if (params.has(WITH_VERSION) && params.get(WITH_VERSION).getAsBoolean())
				visitor.withVersion();
		}

		return e.accept(visitor).getAsJsonObject();
	}
}
