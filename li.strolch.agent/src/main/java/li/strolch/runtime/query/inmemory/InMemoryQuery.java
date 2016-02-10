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
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import li.strolch.model.StrolchElement;
import li.strolch.model.visitor.StrolchElementVisitor;
import li.strolch.persistence.api.StrolchDao;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryQuery<T extends StrolchElement, U> {

	private Navigator<T> navigator;
	private Selector<T> selector;
	private StrolchElementVisitor<T, U> elementVisitor;
	private Comparator<T> comparator;

	public InMemoryQuery() {
		// empty constructor
	}

	public InMemoryQuery(Navigator<T> navigator, Selector<T> selector, StrolchElementVisitor<T, U> elementVisitor,
			Comparator<T> comparator) {
		this.navigator = navigator;
		this.selector = selector;
		this.elementVisitor = elementVisitor;
		this.comparator = comparator;
	}

	/**
	 * @param navigator
	 *            the navigator to set
	 */
	public void setNavigator(Navigator<T> navigator) {
		this.navigator = navigator;
	}

	/**
	 * @param selector
	 *            the selector to set
	 */
	public void setSelector(Selector<T> selector) {
		this.selector = selector;
	}

	/**
	 * @param elementVisitor
	 *            the elementVisitor to set
	 */
	public void setElementVisitor(StrolchElementVisitor<T, U> elementVisitor) {
		this.elementVisitor = elementVisitor;
	}

	public List<U> doQuery(StrolchDao<T> dao) {

		if (this.selector == null)
			return Collections.emptyList();

		List<T> elements = this.navigator.navigate(dao);
		if (this.comparator != null)
			elements.sort(this.comparator);

		List<U> result = new ArrayList<>();
		Iterator<T> iter = elements.iterator();
		while (iter.hasNext()) {
			T element = iter.next();
			if (this.selector.select(element)) {
				U returnValue = this.elementVisitor.visit(element);
				DBC.INTERIM.assertNotNull("Visitor may not return null in query!", returnValue); //$NON-NLS-1$
				result.add(returnValue);
			}
		}

		return result;
	}
}
