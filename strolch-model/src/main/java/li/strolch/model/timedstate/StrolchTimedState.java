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
package li.strolch.model.timedstate;

import java.time.ZonedDateTime;
import java.util.Iterator;
import java.util.NavigableSet;

import li.strolch.model.Resource;
import li.strolch.model.StrolchElement;
import li.strolch.model.StrolchModelConstants;
import li.strolch.model.StrolchValueType;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@SuppressWarnings("rawtypes")
public interface StrolchTimedState<T extends IValue> extends StrolchElement {

	/**
	 * @return the {@link StrolchValueType}
	 */
	StrolchValueType getValueType();

	/**
	 * get the hidden attribute
	 *
	 * @return the hidden value
	 */
	boolean isHidden();

	/**
	 * set the hidden attribute
	 *
	 * @param hidden
	 * 		the new hidden value
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
	 * Returns true if the UOM is not {@link StrolchModelConstants#UOM_NONE}
	 *
	 * @return true if the UOM is not {@link StrolchModelConstants#UOM_NONE}
	 */
	boolean isUomDefined();

	/**
	 * Returns true if the UOM is set to {@link StrolchModelConstants#UOM_NONE}
	 *
	 * @return true if the UOM is set to {@link StrolchModelConstants#UOM_NONE}
	 */
	boolean isUomEmpty();

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
	 * Returns true if the interpretation is set to {@link StrolchModelConstants#INTERPRETATION_NONE}
	 *
	 * @return true if the interpretation is set to {@link StrolchModelConstants#INTERPRETATION_NONE}
	 */
	boolean isInterpretationEmpty();

	ITimeValue<T> getNextMatch(Long time, T value);

	ITimeValue<T> getPreviousMatch(Long time, T value);

	<U extends IValueChange<T>> void applyChange(U change, boolean compact);

	ITimeValue<T> getStateAt(Long time);

	/**
	 * set the value at a point in time to a given time value object from a string value
	 *
	 * @param time
	 * 		the time to set the {@link IValue}
	 * @param value
	 * 		the string to parse to an {@link IValue}
	 */
	void setStateFromStringAt(final Long time, final String value);

	ITimeVariable<T> getTimeEvolution();

	void setParent(Resource aThis);

	@Override
	StrolchTimedState<T> getClone();

	void clear();

	/**
	 * Trims this timed state, so it has at most the given number of values
	 *
	 * @param maxValues
	 * 		the number of values to keep
	 *
	 * @return true if the state was trimmed, false if not
	 */
	default boolean trim(int maxValues) {
		assertNotReadonly();

		ITimeVariable<T> timeEvolution = getTimeEvolution();
		NavigableSet<? extends ITimeValue<?>> values = timeEvolution.getValues();
		if (values.size() < maxValues)
			return false;

		Iterator<? extends ITimeValue<?>> iterator = values.descendingIterator();
		ITimeValue<?> next = iterator.next();
		for (int i = 0; i < maxValues - 1; i++) {
			next = iterator.next();
		}

		return !timeEvolution.removePastValues(next.getTime()).isEmpty();
	}

	/**
	 * Trims this timed state, so all values before the given time stamp
	 *
	 * @param timeStamp
	 * 		the max date the values may have
	 * @param keepLastValue
	 * 		if true, and the last value is before the max
	 *
	 * @return true if the state was trimmed, false if not
	 */
	default boolean trim(ZonedDateTime timeStamp, boolean keepLastValue) {
		assertNotReadonly();
		ITimeVariable<T> timeEvolution = getTimeEvolution();

		long time = timeStamp.toInstant().toEpochMilli();
		if (keepLastValue && timeEvolution.getFutureValues(time).isEmpty()) {
			ITimeValue<T> valueAt = timeEvolution.getValueAt(time);
			if (valueAt == null)
				return false;
			time = valueAt.getTime();
		}

		return !timeEvolution.removePastValues(time).isEmpty();
	}
}
