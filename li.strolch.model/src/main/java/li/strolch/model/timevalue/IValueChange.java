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

/**
 * Interface for operators to be used to change the values of {@link ITimeValue} in a {@link ITimeVariable}.
 *
 * @author Martin Smock <smock.martin@gmail.com>
 */
@SuppressWarnings("rawtypes")
public interface IValueChange<T extends IValue> {

	/**
	 * @return the time this change has to be applied
	 */
	Long getTime();

	/**
	 * @return the value of the change
	 */
	T getValue();

	/**
	 * @return the inverse neutralizing a change. Very useful to undo changes applied.
	 */
	IValueChange<T> getInverse();

}
