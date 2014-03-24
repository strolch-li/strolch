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

import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 *
 */
public interface ListParameter<E> extends Parameter<List<E>> {

	public static final String VALUE_SEPARATOR = ";"; //$NON-NLS-1$

	/**
	 * Adds a single value to the {@link List} of values
	 *
	 * @param value the value to add
	 */
	public void addValue(E value);

	/**
	 * Removes a single value from the {@link List} of values
	 *
	 * @param value the value to remove
	 *
	 * @return true if the value was removed, false if it did not exist
	 */
	public boolean removeValue(E value);
}
