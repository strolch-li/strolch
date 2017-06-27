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
package li.strolch.model.timedstate;

import li.strolch.model.timevalue.ITimeValue;
import li.strolch.model.timevalue.ITimeVariable;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;

/**
 * A time based state characterized by a {@link IValue} object implementation.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 *
 * @param <T>
 *            IValue implementation representing the state at a given time
 */
@SuppressWarnings("rawtypes")
public interface ITimedState<T extends IValue> {

	/**
	 * @return the new {@link ITimeValue} matching the value in the future
	 */
	ITimeValue<T> getNextMatch(final Long time, T value);

	/**
	 * @return the new {@link ITimeValue} matching the value in the past
	 */
	ITimeValue<T> getPreviousMatch(final Long time, T value);

	/**
	 * @param change
	 *            the state change to be applied
	 */
	<U extends IValueChange<T>> void applyChange(final U change, boolean compact);

	/**
	 * @return the state at the given time
	 */
	ITimeValue<T> getStateAt(final Long time);

	/**
	 * @return get the states evolution in time
	 */
	ITimeVariable<T> getTimeEvolution();

	/**
	 * @return a copy of this timed state
	 */
	ITimedState<T> getCopy();
}
