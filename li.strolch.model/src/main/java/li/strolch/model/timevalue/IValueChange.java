/*
 * Copyright 2013 Martin Smock <smock.martin@gmail.com>
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

import li.strolch.model.timedstate.AbstractStrolchTimedState;
import li.strolch.model.timevalue.impl.TimeVariable;

/**
 * Interface for operators to be used to change the values of {@link ITimeValue} in a {@link ITimeVariable} or
 * {@link AbstractStrolchTimedState}.
 * 
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public interface IValueChange<T extends IValue> {

	/**
	 * @return the id of the {@link AbstractStrolchTimedState} the change applies to
	 */
	String getStateId();

	/**
	 * @param id
	 *            the id of the {@link AbstractStrolchTimedState} the change applies to
	 */
	void setStateId(String id);

	/**
	 * @return the time this change has to be applied
	 */
	Long getTime();

	void setTime(Long time);

	/**
	 * @return the value of the change
	 */
	T getValue();

	void setValue(T value);

	/**
	 * @return the inverse neutralizing a change. Very useful to undo changes made to a {@link TimeVariable}.
	 */
	IValueChange<T> getInverse();

	/**
	 * @return a copy of this
	 */
	IValueChange<T> getClone();
}
