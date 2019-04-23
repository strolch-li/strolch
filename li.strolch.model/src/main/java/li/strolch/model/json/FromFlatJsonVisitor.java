package li.strolch.model.json;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import li.strolch.exception.StrolchModelException;
import li.strolch.model.Order;
import li.strolch.model.ParameterBag;
import li.strolch.model.Resource;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Tags.Json;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.utils.collections.MapOfSets;
import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * Maps a given {@link JsonObject} to a {@link StrolchRootElement}. All {@link Parameter Parameters} on the element are
 * iterated and expected to be found as a member on the {@link JsonObject}.
 * </p>
 * <p>
 * To ignore {@link Parameter Parameters} or {@link ParameterBag ParameterBags} use the {@link #ignoreParameter(String,
 * String)} and {@link #ignoreBag(String)} methods
 * </p>
 * <p>
 * {@link Parameter} can be made optional by using the {@link #optionalParameter(String, String)} method
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class FromFlatJsonVisitor implements StrolchRootElementVisitor<Void> {

	private Set<String> ignoredBagTypes;
	private MapOfSets<String, String> ignoredKeys;
	private MapOfSets<String, String> optionalKeys;
	private MapOfSets<String, String> nonEmptyParameters;

	private JsonObject srcObject;

	public FromFlatJsonVisitor() {
		this.ignoredBagTypes = new HashSet<>();
		this.nonEmptyParameters = new MapOfSets<>();
		this.ignoredKeys = new MapOfSets<>();
		this.optionalKeys = new MapOfSets<>();
		this.nonEmptyParameters = new MapOfSets<>();
	}

	public FromFlatJsonVisitor(JsonObject srcObject) {
		this.srcObject = srcObject;
		this.ignoredBagTypes = new HashSet<>();
		this.nonEmptyParameters = new MapOfSets<>();
		this.ignoredKeys = new MapOfSets<>();
		this.optionalKeys = new MapOfSets<>();
	}

	public FromFlatJsonVisitor(MapOfSets<String, String> ignoredParams) {
		this.ignoredKeys = new MapOfSets<>();
		this.optionalKeys = new MapOfSets<>();
	}

	public FromFlatJsonVisitor nonEmptyParameter(String bagId, String paramId) {
		this.nonEmptyParameters.addElement(bagId, paramId);
		return this;
	}

	public FromFlatJsonVisitor ignoreBagsOfType(String bagType) {
		this.ignoredBagTypes.add(bagType);
		return this;
	}

	public FromFlatJsonVisitor ignoreBag(String bagId) {
		this.ignoredKeys.addSet(bagId, Collections.emptySet());
		return this;
	}

	public FromFlatJsonVisitor ignoreParameter(String bagId, String paramId) {
		this.ignoredKeys.addElement(bagId, paramId);
		return this;
	}

	public FromFlatJsonVisitor optionalParameter(String bagId, String paramId) {
		this.optionalKeys.addElement(bagId, paramId);
		return this;
	}

	@Override
	public Void visitResource(Resource dstElement) {
		if (this.srcObject.has(Json.OBJECT_TYPE)) {
			DBC.PRE.assertEquals("objectType must be the same, if set on JsonObject!", dstElement.getObjectType(),
					srcObject.get(Json.OBJECT_TYPE).getAsString());
		}

		visit(dstElement);
		return null;
	}

	@Override
	public Void visitOrder(Order dstElement) {
		if (this.srcObject.has(Json.OBJECT_TYPE)) {
			DBC.PRE.assertEquals("objectType must be the same, if set on JsonObject!", dstElement.getObjectType(),
					srcObject.get(Json.OBJECT_TYPE).getAsString());
		}

		visit(dstElement);
		return null;
	}

	@Override
	public Void visitActivity(Activity dstElement) {
		throw new UnsupportedOperationException("Activity elements are not supported by this visitor!");
	}

	public void visit(StrolchRootElement dstElement, JsonObject srcObject) {
		if (this.srcObject != null && this.srcObject != srcObject) {
			throw new IllegalStateException("The srcObject was already set with a different value in the constructor!");
		}

		DBC.PRE.assertTrue("objectType must be set!", srcObject.has(Json.OBJECT_TYPE));
		DBC.PRE.assertEquals("objectType must be the same!", dstElement.getObjectType(),
				srcObject.get(Json.OBJECT_TYPE).getAsString());

		this.srcObject = srcObject;
		visit(dstElement);
	}

	public void visit(StrolchRootElement dstElement) {

		// types must be the same, if exists on source element
		if (this.srcObject.has(Json.TYPE))
			DBC.PRE.assertEquals("type must be the same!", dstElement.getType(),
					this.srcObject.get(Json.TYPE).getAsString());

		// update name if possible
		if (this.srcObject.has(Json.NAME))
			dstElement.setName(this.srcObject.get(Json.NAME).getAsString());

		Set<String> bagKeySet = dstElement.getParameterBagKeySet();
		for (String bagId : bagKeySet) {

			// see if we have to ignore this bag i.e. empty set existing
			Set<String> ignoredParamIds = this.ignoredKeys.getSet(bagId);
			if (ignoredParamIds != null && ignoredParamIds.isEmpty())
				continue;

			ParameterBag parameterBag = dstElement.getParameterBag(bagId);

			// see if we want to ignore bags of this type
			if (this.ignoredBagTypes.contains(parameterBag.getType()))
				continue;

			Set<String> parameterKeySet = parameterBag.getParameterKeySet();
			for (String paramId : parameterKeySet) {

				// see if this parameter must be ignored
				if (ignoredParamIds != null && ignoredParamIds.contains(paramId))
					continue;

				JsonElement jsonElement = this.srcObject.get(paramId);
				if (jsonElement == null) {

					if (this.optionalKeys.containsElement(bagId, paramId))
						continue;

					throw new StrolchModelException(
							"JsonObject is missing member " + paramId + " for " + parameterBag.getLocator() + "/"
									+ paramId);
				}

				if (!jsonElement.isJsonPrimitive()) {
					throw new StrolchModelException(
							"JsonElement " + paramId + " is not a json primitive but a " + jsonElement.getClass()
									.getName() + " for " + parameterBag.getLocator() + "/" + paramId);
				}

				Parameter<?> parameter = parameterBag.getParameter(paramId);

				String asString = jsonElement.getAsString();
				if (asString.isEmpty() && this.nonEmptyParameters.containsElement(bagId, paramId)) {
					throw new StrolchModelException(
							"JsonElement " + paramId + " is required to be a non empty value for " + parameter
									.getLocator());
				}

				try {
					parameter.setValueFromString(asString);
				} catch (Exception e) {
					throw new IllegalStateException("Failed to set parameter " + parameter.getLocator(), e);
				}
			}
		}
	}
}
