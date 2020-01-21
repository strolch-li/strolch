/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package li.strolch.model;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_NONE;
import static li.strolch.model.StrolchModelConstants.UOM_NONE;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import li.strolch.model.activity.Activity;
import li.strolch.model.json.StrolchRootElementToJsonVisitor;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.visitor.SetParameterValueVisitor;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.model.visitor.StrolchRootElementVisitor;
import li.strolch.model.xml.StrolchElementToXmlStringVisitor;

/**
 * Root element for all top level {@link StrolchElement}. These are elements which have no parent, e.g. {@link Resource
 * Resources} and {@link Order Orders}. Every root element has a version, so that versions can be kept of an object
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface StrolchRootElement extends StrolchElement, PolicyContainer, ParameterBagContainer {

	/**
	 * Returns the object type
	 *
	 * @return the object type
	 */
	String getObjectType();

	/**
	 * Set the type of this {@link StrolchRootElement}. Not that this method should only be called for new elements, not
	 * if this element has already been persisted!
	 *
	 * @param type
	 * 		the new type
	 */
	void setType(String type);

	/**
	 * Returns true if this {@link StrolchRootElement} has a version set
	 *
	 * @return true if this {@link StrolchRootElement} has a version set
	 */
	boolean hasVersion();

	/**
	 * Returns the current version of this object, or null if no version is set
	 *
	 * @return the current version of this object, or null if no version is set
	 */
	Version getVersion();

	/**
	 * <p>
	 * Sets the version of this object
	 * </p>
	 *
	 * @param version
	 * 		the version to set
	 *
	 * @throws IllegalArgumentException
	 * 		if the given version's locator is not equal to the current element's locator
	 */
	void setVersion(Version version) throws IllegalArgumentException;

	/**
	 * Return a clone of this {@link StrolchElement}
	 *
	 * @return a clone of this {@link StrolchElement}
	 */
	@Override
	StrolchRootElement getClone();

	/**
	 * Return a clone of this {@link StrolchElement}
	 *
	 * @return a clone of this {@link StrolchElement}
	 */
	StrolchRootElement getClone(boolean withVersion);

	/**
	 * Visitor pattern accept method. Takes a {@link StrolchRootElementVisitor} to visit this element
	 *
	 * @param visitor
	 * 		the visitor
	 *
	 * @return the result of the visitation
	 */
	<T> T accept(StrolchElementVisitor<T> visitor);

	/**
	 * Formats this {@link StrolchRootElement} as an XML string
	 *
	 * @return the formatted XML string
	 */
	default String toXmlString() {
		return accept(new StrolchElementToXmlStringVisitor().withoutDocument());
	}

	/**
	 * Formats this element as a JSON string
	 *
	 * @return the formatted JSON string
	 */
	default String toJsonString() {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(this.accept(new StrolchRootElementToJsonVisitor()));
	}

	/**
	 * Formats this element as a flat JSON string, i.e. all parameter bags are removed and parameters are on the root
	 * element of the JSON object
	 *
	 * @return the formatted JSON string
	 */
	default String toFlatJsonString() {
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		return gson.toJson(this.accept(new StrolchRootElementToJsonVisitor().flat()));
	}

	/**
	 * Formats this element to a {@link JsonObject}
	 *
	 * @return the {@link JsonObject}
	 */
	default JsonObject toJsonObject() {
		return this.accept(new StrolchRootElementToJsonVisitor());
	}

	/**
	 * Formats this element to a flat {@link JsonObject}
	 *
	 * @return the {@link JsonObject}
	 */
	default JsonObject toFlatJsonObject() {
		return this.accept(new StrolchRootElementToJsonVisitor().flat());
	}

	/**
	 * Casts this element to a Resource
	 *
	 * @return Casts this element to a Resource
	 */
	default Resource asResource() {
		return (Resource) this;
	}

	/**
	 * Casts this element to a Order
	 *
	 * @return Casts this element to a Order
	 */
	default Order asOrder() {
		return (Order) this;
	}

	/**
	 * Casts this element to a Activity
	 *
	 * @return Casts this element to a Activity
	 */
	default Activity asActivity() {
		return (Activity) this;
	}

	/**
	 * Returns true if this element is a {@link Resource}
	 *
	 * @return true if this element is a {@link Resource}
	 */
	default boolean isResource() {
		return this.getClass() == Resource.class;
	}

	/**
	 * Returns true if this element is a {@link Order}
	 *
	 * @return true if this element is a {@link Order}
	 */
	default boolean isOrder() {
		return this.getClass() == Order.class;
	}

	/**
	 * Returns true if this element is a {@link Activity}
	 *
	 * @return true if this element is a {@link Activity}
	 */
	default boolean isActivity() {
		return this.getClass() == Activity.class;
	}

	/**
	 * Set or add a parameter to this element from a {@link JsonObject}
	 *
	 * @param jsonObject
	 * 		the object from which to get the value
	 * @param bagId
	 * 		the bag ID on which to set the value
	 * @param bagName
	 * 		the name of the bag, if the bag is to be created
	 * @param bagType
	 * 		the type of the bag, if the bag is to be created
	 * @param paramId
	 * 		the ID of the parameter on which to set the value, and also the Json reference ID
	 * @param paramName
	 * 		the name of the parameter, if the parameter is to be created
	 * @param type
	 * 		the type of Parameter to create
	 * @param ignoreOnEmpty
	 * 		if true, and the json object is missing the field, then the parameter is not changed, otherwise the parameter
	 * 		is cleared if the json field is missing or null
	 */
	default void setOrAddParamFromFlatJson(JsonObject jsonObject, String bagId, String bagName, String bagType,
			String paramId, String paramName, StrolchValueType type, boolean ignoreOnEmpty) {
		setOrAddParamFromFlatJson(jsonObject, bagId, bagName, bagType, paramId, paramName, INTERPRETATION_NONE,
				UOM_NONE, type, ignoreOnEmpty);
	}

	/**
	 * Set or add a parameter to this element from a {@link JsonObject}
	 *
	 * @param jsonObject
	 * 		the object from which to get the value
	 * @param bagId
	 * 		the bag ID on which to set the value
	 * @param bagName
	 * 		the name of the bag, if the bag is to be created
	 * @param bagType
	 * 		the type of the bag, if the bag is to be created
	 * @param paramId
	 * 		the ID of the parameter on which to set the value, and also the Json reference ID
	 * @param paramName
	 * 		the name of the parameter, if the parameter is to be created
	 * @param interpretation
	 * 		the interpretation
	 * @param uom
	 * 		the uom
	 * @param type
	 * 		the type of Parameter to create
	 * @param ignoreOnEmpty
	 * 		if true, and the json object is missing the field, then the parameter is not changed, otherwise the parameter
	 * 		is cleared if the json field is missing or null
	 */
	default void setOrAddParamFromFlatJson(JsonObject jsonObject, String bagId, String bagName, String bagType,
			String paramId, String paramName, String interpretation, String uom, StrolchValueType type,
			boolean ignoreOnEmpty) {

		if (!jsonObject.has(paramId) && ignoreOnEmpty)
			return;

		ParameterBag bag = getParameterBag(bagId);
		if (bag == null) {
			bag = new ParameterBag(bagId, bagName, bagType);
			addParameterBag(bag);
		}

		Parameter<?> param = bag.getParameter(paramId);
		boolean valueNotSet = !jsonObject.has(paramId) || jsonObject.get(paramId).isJsonNull();
		if (param == null && valueNotSet)
			return;

		if (param == null) {
			param = type.parameterInstance();
			param.setId(paramId);
			param.setName(paramName);
			param.setInterpretation(interpretation);
			param.setUom(uom);
			bag.addParameter(param);
		}

		if (valueNotSet) {
			param.clear();
		} else {
			param.setValueFromString(jsonObject.get(paramId).getAsString());
		}
	}

	/**
	 * Set or add a parameter to this element with the given value
	 *
	 * @param bagId
	 * 		the bag ID on which to set the value
	 * @param bagName
	 * 		the name of the bag, if the bag is to be created
	 * @param bagType
	 * 		the type of the bag, if the bag is to be created
	 * @param paramId
	 * 		the ID of the parameter on which to set the value, and also the Json reference ID
	 * @param paramName
	 * 		the name of the parameter, if the parameter is to be created
	 * @param type
	 * 		the type of Parameter to create
	 * @param value
	 * 		the value to set
	 * @param ignoreOnEmpty
	 * 		if true, and the value is null, then the parameter is not changed, otherwise the parameter is cleared if the
	 * 		value is null
	 */
	default void setOrAddParam(String bagId, String bagName, String bagType, String paramId, String paramName,
			StrolchValueType type, Object value, boolean ignoreOnEmpty) {
		setOrAddParam(bagId, bagName, bagType, paramId, paramName, INTERPRETATION_NONE, UOM_NONE, type, value,
				ignoreOnEmpty);
	}

	/**
	 * Set or add a parameter to this element with the given value
	 *
	 * @param bagId
	 * 		the bag ID on which to set the value
	 * @param bagName
	 * 		the name of the bag, if the bag is to be created
	 * @param bagType
	 * 		the type of the bag, if the bag is to be created
	 * @param paramId
	 * 		the ID of the parameter on which to set the value, and also the Json reference ID
	 * @param paramName
	 * 		the name of the parameter, if the parameter is to be created
	 * @param interpretation
	 * 		the interpretation
	 * @param uom
	 * 		the uom
	 * @param type
	 * 		the type of Parameter to create
	 * @param value
	 * 		the value to set
	 * @param ignoreOnEmpty
	 * 		if true, and the value is null, then the parameter is not changed, otherwise the parameter is cleared if the
	 * 		value is null
	 */
	default void setOrAddParam(String bagId, String bagName, String bagType, String paramId, String paramName,
			String interpretation, String uom, StrolchValueType type, Object value, boolean ignoreOnEmpty) {

		if (value == null && ignoreOnEmpty)
			return;

		ParameterBag bag = getParameterBag(bagId);
		if (bag == null) {
			bag = new ParameterBag(bagId, bagName, bagType);
			addParameterBag(bag);
		}

		Parameter<?> param = bag.getParameter(paramId);
		if (param == null && value == null)
			return;

		if (param == null) {
			param = type.parameterInstance();
			param.setId(paramId);
			param.setName(paramName);
			param.setInterpretation(interpretation);
			param.setUom(uom);
			bag.addParameter(param);
		}

		if (value == null) {
			param.clear();
		} else {
			param.accept(new SetParameterValueVisitor(value));
		}
	}
}
