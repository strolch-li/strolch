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

import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * A {@link Parameter} which supports a list of elements.
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface ListParameter<E> extends Parameter<List<E>> {

	String VALUE_SEPARATOR1 = ";";
	String VALUE_SEPARATOR2 = ",";

	/**
	 * Returns the value at the given index of this {@link ListParameter}'s value
	 *
	 * @param index
	 * 		the index from which to return the value
	 *
	 * @return the value at the given index
	 */
	E getValue(int index);

	/**
	 * Returns a {@link Stream} for the values of this list
	 *
	 * @return a stream for the values
	 */
	Stream<E> streamValues();

	/**
	 * Set the internal value to have the content of the collection
	 *
	 * @param values
	 * 		the values to set
	 */
	void setValue(Collection<E> values);

	/**
	 * Adds a single value to the {@link List} of values
	 *
	 * @param value
	 * 		the value to add
	 */
	void addValue(E value);

	/**
	 * Adds the given values to the {@link List} of values
	 *
	 * @param values
	 * 		the values to add
	 */
	void addAllValues(List<E> values);

	/**
	 * Adds all of the given values to the {@link List} of values which are not already in the current list
	 *
	 * @param values
	 * 		the values to add
	 */
	void addAllValuesIfNotContains(List<E> values);

	/**
	 * Adds a single value to the {@link List} of values if the current list does not already contain the value
	 *
	 * @param value
	 * 		the value to add
	 */
	boolean addValueIfNotContains(E value);

	/**
	 * Removes a single value from the {@link List} of values
	 *
	 * @param value
	 * 		the value to remove
	 *
	 * @return true if the value was removed, false if it did not exist
	 */
	boolean removeValue(E value);

	/**
	 * Removes all values from the {@link List} of values if the predicate passes
	 *
	 * @param predicate
	 * 		the predicate to evaluate if the value should be removed
	 *
	 * @return true if any values were removed
	 */
	boolean removeValueIf(Predicate<E> predicate);

	/**
	 * Clears the list of values, i.e the list of values is empty after this call
	 */
	void clear();

	/**
	 * Returns true if the list of values is empty, false if not
	 *
	 * @return true if the list of values is empty, false if not
	 */
	boolean isEmpty();

	/**
	 * Returns the size of the list of values
	 *
	 * @return the size of the list of values
	 */
	int size();

	/**
	 * Returns true if the list of values contains the given element, false if not
	 *
	 * @param value
	 * 		the value to check if it is contained in the list of values
	 *
	 * @return true if the list of values contains the given element, false if not
	 */
	boolean contains(E value);

	/**
	 * Returns true if the list of values contains all of the given elements, false if not
	 *
	 * @param values
	 * 		the values to check if they are contained in the list of values
	 *
	 * @return true if the list of values contains all of the given elements, false if not
	 */
	boolean containsAll(List<E> values);
}
