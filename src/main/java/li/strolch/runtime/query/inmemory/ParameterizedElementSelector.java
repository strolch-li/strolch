/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.runtime.query.inmemory;

import java.util.List;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.ParameterBag;
import li.strolch.model.ParameterizedElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ParameterizedElementSelector<T extends GroupedParameterizedElement> implements Selector<T> {

	private String key;
	private List<Selector<ParameterizedElement>> selectors;

	public ParameterizedElementSelector(String key, List<Selector<ParameterizedElement>> selectors) {
		this.key = key;
		this.selectors = selectors;
	}

	@Override
	public boolean select(T element) {

		ParameterBag parameterBag = element.getParameterBag(this.key);
		if (parameterBag == null)
			return false;

		for (Selector<ParameterizedElement> selector : this.selectors) {
			if (!selector.select(parameterBag))
				return false;
		}

		return true;
	}
}
