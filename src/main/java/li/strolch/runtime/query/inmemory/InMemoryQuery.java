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
package li.strolch.runtime.query.inmemory;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryQuery<T extends StrolchElement> {

	private Navigator<T> navigator;
	private List<Selector<T>> selectors;

	public InMemoryQuery() {
		this.selectors = new ArrayList<>();
	}

	public InMemoryQuery(Navigator<T> navigator, List<Selector<T>> selectors) {
		this.navigator = navigator;
		this.selectors = selectors;
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

	public List<T> doQuery(StrolchTransaction tx) {

		List<T> elements = this.navigator.navigate(tx);
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
