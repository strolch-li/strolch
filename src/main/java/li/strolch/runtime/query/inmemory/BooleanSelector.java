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
import java.util.Arrays;
import java.util.List;

import li.strolch.model.StrolchElement;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class BooleanSelector<T extends StrolchElement> implements Selector<T> {

	protected List<Selector<T>> selectors;

	public BooleanSelector() {
		this.selectors = new ArrayList<>(1);
	}

	@SafeVarargs
	public BooleanSelector(Selector<T>... selector) {
		this.selectors = Arrays.asList(selector);
	}

	public BooleanSelector(Selector<T> leftHandSide, Selector<T> rightHandSide) {
		this.selectors = new ArrayList<>(2);
		this.selectors.add(leftHandSide);
		this.selectors.add(rightHandSide);
	}

	public BooleanSelector(List<Selector<T>> selectors) {
		this.selectors = selectors;
	}

	public BooleanSelector<T> with(Selector<T> selector) {
		this.selectors.add(selector);
		return this;
	}

	@Override
	public abstract boolean select(T element);
}
