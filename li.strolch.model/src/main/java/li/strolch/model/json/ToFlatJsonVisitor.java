package li.strolch.model.json;

import static li.strolch.model.Tags.Json.ID;
import static li.strolch.model.Tags.Json.NAME;
import static li.strolch.model.Tags.Json.OBJECT_TYPE;
import static li.strolch.model.Tags.Json.TYPE;

import java.util.Collections;
import java.util.Set;
import java.util.function.BiConsumer;

import com.google.gson.JsonObject;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.ParameterBag;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.Parameter;
import li.strolch.utils.collections.MapOfSets;

/**
 * <p>
 * Maps a given {@link StrolchRootElement} to a {@link JsonObject}. All {@link Parameter Parameters} are mapped to a
 * member of the {@link JsonObject}, {@link ParameterBag ParameterBags} are thus removed
 * </p>
 * 
 * <p>
 * To ignore {@link Parameter Parameters} or {@link ParameterBag ParameterBags} use the
 * {@link #ignoreParameter(String, String)} and {@link #ignoreBag(String)} methods
 * </p>
 * 
 * <p>
 * The mapping can be extended by setting hook:
 * </p>
 * 
 * <pre>
 * visitor.setHook((element, jsonObject) -> {
 * 	// do work
 * });
 * </pre>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ToFlatJsonVisitor<T extends StrolchRootElement> {

	private MapOfSets<String, String> ignoredKeys;
	private BiConsumer<T, JsonObject> hook;
	private boolean withoutElementName;

	public ToFlatJsonVisitor() {
		this.ignoredKeys = new MapOfSets<>();
	}

	public ToFlatJsonVisitor(MapOfSets<String, String> ignoredParams) {
		this.ignoredKeys = new MapOfSets<>();
	}

	public ToFlatJsonVisitor<T> withoutElementName() {
		this.withoutElementName = true;
		return this;
	}

	public ToFlatJsonVisitor<T> setHook(BiConsumer<T, JsonObject> hook) {
		this.hook = hook;
		return this;
	}

	public ToFlatJsonVisitor<T> ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
		return this;
	}

	public ToFlatJsonVisitor<T> ignoreParameter(String bagId, String paramId) {
		this.ignoredKeys.addElement(bagId, paramId);
		return this;
	}

	public JsonObject toJson(T element) {

		JsonObject jsonObject = new JsonObject();

		jsonObject.addProperty(ID, element.getId());
		if (!this.withoutElementName)
			jsonObject.addProperty(NAME, element.getName());
		jsonObject.addProperty(TYPE, element.getType());
		jsonObject.addProperty(OBJECT_TYPE, element.getClass().getSimpleName());

		Set<String> bagKeySet = element.getParameterBagKeySet();
		for (String bagId : bagKeySet) {

			// see if we have to ignore this bag i.e. empty set existing
			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bagId);
			if (ignoredParamIds != null && ignoredParamIds.isEmpty())
				continue;

			ParameterBag parameterBag = element.getParameterBag(bagId);

			Set<String> parameterKeySet = parameterBag.getParameterKeySet();
			for (String paramId : parameterKeySet) {

				// see if this parameter must be ignored
				if (ignoredParamIds != null && ignoredParamIds.contains(paramId))
					continue;

				if (jsonObject.has(paramId)) {
					throw new StrolchModelException(
							"JsonObject already has a member with ID " + paramId + ": " + parameterBag.getLocator());
				}

				Parameter<?> param = parameterBag.getParameter(paramId);

				StrolchValueType type = StrolchValueType.parse(param.getType());
				if (type.isBoolean()) {
					jsonObject.addProperty(paramId, (Boolean) param.getValue());
				} else if (type.isNumber()) {
					jsonObject.addProperty(paramId, (Number) param.getValue());
				} else {
					jsonObject.addProperty(paramId, param.getValueAsString());
				}
			}
		}

		if (hook != null)
			this.hook.accept(element, jsonObject);

		return jsonObject;
	}
}
