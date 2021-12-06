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
package li.strolch.model.parameter;

import li.strolch.model.ParameterizedElement;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchValueType;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface Parameter<T> extends StrolchElement, Comparable<Parameter<?>> {

	/**
	 * Returns the value of the parameter as string
	 *
	 * @return the value as string
	 */
	String getValueAsString();

	/**
	 * Set the value of the parameter from a string
	 *
	 * @param valueAsString
	 * 		the string from which to set the value
	 */
	void setValueFromString(String valueAsString);

	/**
	 * the value of the parameter
	 *
	 * @return the value
	 */
	<U extends T> U getValue();

	/**
	 * set the value of the parameter
	 *
	 * @param value
	 * 		the new value
	 */
	void setValue(T value);

	/**
	 * set the value of the parameter from another value, i.e. copying the value
	 *
	 * @param parameter
	 * 		the parameter from which to copy the new value
	 */
	void setValueFrom(Parameter<T> parameter);

	/**
	 * Clears the value, dependent on the concrete class
	 */
	void clear();

	/**
	 * @return true if the value is empty, i.e. if the value is the same as the value which would be set if {@link
	 * #clear()} was called
	 */
	boolean isEmpty();

	/**
	 * Returns true if the value is set, i.e. not empty. This is the inverse of {@link #isEmpty()}
	 *
	 * @return true if the value is set, i.e. not empty
	 */
	default boolean isSet() {
		return !isEmpty();
	}

	/**
	 * Returns true if the given parameter's value is equal to the current value
	 *
	 * @param otherValue
	 * 		the value to check on equality
	 *
	 * @return true if the given parameter's value is equal to the current value
	 */
	boolean isEqualTo(Parameter<T> otherValue);

	/**
	 * Returns true if the given value is equal to the current value
	 *
	 * @param otherValue
	 * 		the value to check on equality
	 *
	 * @return true if the given value is equal to the current value
	 */
	boolean isEqualTo(T otherValue);

	/**
	 * get the hidden attribute
	 *
	 * @return hidden value
	 */
	boolean isHidden();

	/**
	 * set the hidden attribute
	 *
	 * @param hidden
	 * 		new hidden value
	 */
	void setHidden(boolean hidden);

	/**
	 * Get the UOM of this {@link Parameter}
	 *
	 * @return the UOM
	 */
	String getUom();

	/**
	 * Set the UOM of this {@link Parameter}
	 *
	 * @param uom
	 * 		the new UOM
	 */
	void setUom(String uom);

	/**
	 * Returns true if the interpretation is not {@link StrolchModelConstants#UOM_NONE}
	 *
	 * @return true if the interpretation is not {@link StrolchModelConstants#UOM_NONE}
	 */
	boolean isUomDefined();

	/**
	 * Returns the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 *
	 * @return the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 */
	int getIndex();

	/**
	 * Set the index of this {@link Parameter}. This can be used to sort the parameters in a UI
	 *
	 * @param index
	 * 		the index to set
	 */
	void setIndex(int index);

	/**
	 * Returns the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of
	 * this {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_NONE}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 *
	 * @return string value
	 */
	String getInterpretation();

	/**
	 * Set the interpretation of this {@link Parameter}. The interpretation semantic describes what the value of this
	 * {@link Parameter} means. Currently there are three definitions, but any String value can be used:
	 * <ul>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_NONE}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_ORDER_REF}</li>
	 * <li>{@link StrolchModelConstants#INTERPRETATION_RESOURCE_REF}</li>
	 * </ul>
	 *
	 * @param interpretation
	 * 		the interpretation
	 */
	void setInterpretation(String interpretation);

	/**
	 * Returns true if the interpretation is not {@link StrolchModelConstants#INTERPRETATION_NONE}
	 *
	 * @return true if the interpretation is not {@link StrolchModelConstants#INTERPRETATION_NONE}
	 */
	boolean isInterpretationDefined();

	/**
	 * The {@link ParameterizedElement} parent to which this {@link Parameter} belongs
	 *
	 * @return the parent
	 */
	@Override
	ParameterizedElement getParent();

	/**
	 * Sets the parent for this {@link Parameter}
	 */
	void setParent(ParameterizedElement parent);

	@Override
	int hashCode();

	@Override
	boolean equals(Object obj);

	/**
	 * Compares the value of the given parameter to this parameter
	 *
	 * @param o
	 * 		the parameter for which the value is to be compared to
	 *
	 * @return the {@link Comparable#compareTo(Object)} result
	 */
	@Override
	int compareTo(Parameter<?> o);

	@Override
	Parameter<T> getClone();

	/**
	 * @return the {@link StrolchValueType}
	 */
	StrolchValueType getValueType();
}
