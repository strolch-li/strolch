package li.strolch.rest.visitor;

import java.util.Collections;
import java.util.Set;

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
 * member of the {@link JsonObject}.
 * </p>
 * 
 * <p>
 * To not map a {@link Parameter} add it to the {@link MapOfSets}, to ignore a whole {@link ParameterBag} then add an
 * empty set with the bag id.
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ToFlatJsonVisitor {

	private MapOfSets<String, String> ignoredKeys;

	public ToFlatJsonVisitor() {
		this.ignoredKeys = new MapOfSets<>();
	}

	public ToFlatJsonVisitor(MapOfSets<String, String> ignoredParams) {
		this.ignoredKeys = new MapOfSets<>();
	}

	public void ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
	}

	public void ignoreParameter(String bagId, String paramId) {
		this.ignoredKeys.addElement(bagId, paramId);
	}

	public JsonObject toJson(StrolchRootElement element) {

		JsonObject jsonObject = new JsonObject();

		Set<String> bagKeySet = element.getParameterBagKeySet();
		for (String bagId : bagKeySet) {

			// see if we have to ignore this bag i.e. empty set existing
			Set<String> paramIds = this.ignoredKeys.getSet(bagId);
			if (paramIds != null && paramIds.isEmpty())
				continue;

			ParameterBag parameterBag = element.getParameterBag(bagId);

			Set<String> parameterKeySet = parameterBag.getParameterKeySet();
			for (String paramId : parameterKeySet) {

				// see if this parameter must be ignored
				if (paramIds != null && paramIds.contains(paramId))
					continue;

				if (jsonObject.has(paramId)) {
					throw new StrolchModelException("JsonObject already has a member with ID " + paramId);
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

		return jsonObject;
	}
}
