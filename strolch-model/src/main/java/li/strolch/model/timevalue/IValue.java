/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.model.timevalue;

import li.strolch.model.visitor.IValueVisitor;

/**
 * A value object defining some basic algebraic operations. Mathematically speaking {@link IValue} objects define a
 * group with a addition operation.
 *
 * @param <T> any object for which a (generalized) add operation can be defined.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
public interface IValue<T> extends Comparable<IValue<T>> {

	/**
	 * @return the type of this {@link IValue}
	 */
	String getType();

	/**
	 * @return the backing value
	 */
	T getValue();

	/**
	 * @return a value with the backing value added to the argument value
	 */
	IValue<T> add(T o);

	/**
	 * @return true, if the backing values match.
	 */
	boolean matches(IValue<T> other);

	/**
	 * @return the inverse value, such that add(value.getInverse()) returns the neutral element of the group
	 */
	IValue<T> getInverse();

	/**
	 * @return a copy of this
	 */
	IValue<T> getCopy();

	/**
	 * @return this value in string representation
	 */
	String getValueAsString();

	/**
	 * Accepts a visitor implementing the {@link IValueVisitor} interface and performs an operation defined by the
	 * visitor. This method enables the implementation of the Visitor design pattern to separate operations from the
	 * structure.
	 *
	 * @param <U>     the type of the result returned by the visitor
	 * @param visitor the visitor instance implementing the operation to be performed
	 *
	 * @return the result of the operation defined by the visitor
	 */
	<U> U accept(IValueVisitor<U> visitor);
}
