/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the li.strolch.model.
 *
 *  li.strolch.model is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  li.strolch.model is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with li.strolch.model.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.model.parameter;

import java.util.List;


/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public interface ListParameter<E> extends Parameter<List<E>> {

	/**
	 * Adds a single value to the {@link List} of values
	 * 
	 * @param value
	 *            the value to add
	 */
	public void addValue(E value);

	/**
	 * Removes a single value from the {@link List} of values
	 * 
	 * @param value
	 *            the value to remove
	 * 
	 * @return true if the value was removed, false if it did not exist
	 */
	public boolean removeValue(E value);
}
