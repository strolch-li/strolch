package li.strolch.model.json;

import java.util.Collections;
import java.util.Set;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import li.strolch.exception.StrolchModelException;
import li.strolch.model.ParameterBag;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.utils.collections.MapOfSets;

/**
 * <p>
 * Maps a given {@link JsonObject} to a {@link StrolchRootElement}. All {@link Parameter Parameters} on the element are
 * iterated and expected to be found as a member on the {@link JsonObject}.
 * </p>
 * 
 * <p>
 * To ignore {@link Parameter Parameters} or {@link ParameterBag ParameterBags} use the
 * {@link #ignoreParameter(String, String)} and {@link #ignoreBag(String)} methods
 * </p>
 * 
 * <p>
 * {@link Parameter} can be made optional by using the {@link #optionalParameter(String, String)} method
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 * @param <T>
 */
public class FromFlatJsonVisitor {

	private MapOfSets<String, String> ignoredKeys;
	private MapOfSets<String, String> optionalKeys;

	public FromFlatJsonVisitor() {
		this.ignoredKeys = new MapOfSets<>();
		this.optionalKeys = new MapOfSets<>();
	}

	public FromFlatJsonVisitor(MapOfSets<String, String> ignoredParams) {
		this.ignoredKeys = new MapOfSets<>();
		this.optionalKeys = new MapOfSets<>();
	}

	public void ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
	}

	public void ignoreParameter(String bagId, String paramId) {
		this.ignoredKeys.addElement(bagId, paramId);
	}

	public void optionalParameter(String bagId, String paramId) {
		this.optionalKeys.addElement(bagId, paramId);
	}

	public void visit(StrolchRootElement element, JsonObject jsonObject) {

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

				JsonElement jsonElement = jsonObject.get(paramId);
				if (jsonElement == null) {
					if (this.optionalKeys.containsElement(bagId, paramId))
						continue;
					throw new StrolchModelException("JsonObject is missing member with ID " + paramId);
				}

				if (!jsonElement.isJsonPrimitive()) {
					throw new StrolchModelException("JsonElement " + paramId + " is not a json primitive but a "
							+ jsonElement.getClass().getName());
				}

				Parameter<?> parameter = parameterBag.getParameter(paramId);
				parameter.setValueFromString(jsonElement.getAsString());
			}
		}
	}
}
