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

import li.strolch.model.timevalue.impl.TimeVariable;

/**
 * Interface for timed value objects to be used with the {@link TimeVariable}
 *
 * @param <T>
 * 		the backing value of the timed value object
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public interface ITimeValue<T extends IValue> extends Comparable<ITimeValue<T>> {

	ITimeValue<T> setValue(final T value);

	T getValue();

	Long getTime();

	ITimeValue<T> add(final T change);

	ITimeValue<T> getCopy();
}
