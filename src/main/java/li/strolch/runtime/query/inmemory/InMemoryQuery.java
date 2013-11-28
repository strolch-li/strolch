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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class InMemoryQuery<T extends StrolchElement> {

	private Navigator<T> navigator;
	private List<Selector<T>> selectors;

	public InMemoryQuery() {
		this.selectors = new ArrayList<>();
	}

	/**
	 * @param navigator
	 *            the navigator to set
	 */
	public void setNavigator(Navigator<T> navigator) {
		this.navigator = navigator;
	}

	public void addSelector(Selector<T> selector) {
		this.selectors.add(selector);
	}

	public List<T> doQuery() {

		List<T> elements = this.navigator.navigate();
		Iterator<T> iter = elements.iterator();
		while (iter.hasNext()) {
			T element = iter.next();
			if (!isAccepted(element)) {
				iter.remove();
			}
		}

		return elements;
	}

	private boolean isAccepted(T element) {
		for (Selector<T> selector : this.selectors) {
			if (!selector.select(element))
				return false;
		}

		return true;
	}
}
