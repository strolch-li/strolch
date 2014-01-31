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

import java.util.List;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class OrSelector<T extends StrolchElement> extends BooleanSelector<T> {

	public OrSelector() {
		super();
	}

	public OrSelector(List<Selector<T>> selectors) {
		super(selectors);
	}

	@SafeVarargs
	public OrSelector(Selector<T>... selector) {
		super(selector);
	}

	public OrSelector(Selector<T> leftHandSide, Selector<T> rightHandSide) {
		super(leftHandSide, rightHandSide);
	}

	@Override
	public OrSelector<T> with(Selector<T> selector) {
		super.with(selector);
		return this;
	}

	@Override
	public boolean select(T element) {
		if (this.selectors == null || this.selectors.isEmpty())
			return true;

		for (Selector<T> selector : this.selectors) {
			if (selector.select(element))
				return true;
		}

		return false;
	}
}
