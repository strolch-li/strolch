/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import java.util.Comparator;

import li.strolch.model.GroupedParameterizedElement;
import li.strolch.model.query.ordering.ByIdComparator;
import li.strolch.model.query.ordering.ByNameComparator;
import li.strolch.model.query.ordering.ByParamComparator;
import li.strolch.model.query.ordering.OrderById;
import li.strolch.model.query.ordering.OrderByName;
import li.strolch.model.query.ordering.OrderByParameter;
import li.strolch.model.query.ordering.StrolchQueryOrderingVisitor;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryStrolchQueryOrderingVisitor<T extends GroupedParameterizedElement> implements
		StrolchQueryOrderingVisitor {

	private Comparator<T> comparator;

	public Comparator<T> getComparator() {
		return this.comparator;
	}

	@Override
	public InMemoryStrolchQueryOrderingVisitor<T> visit(OrderById ordering) {
		this.comparator = new ByIdComparator<>(ordering.isAscending());
		return this;
	}

	@Override
	public InMemoryStrolchQueryOrderingVisitor<T> visit(OrderByName ordering) {
		this.comparator = new ByNameComparator<>(ordering.isAscending());
		return this;
	}

	@Override
	public InMemoryStrolchQueryOrderingVisitor<T> visit(OrderByParameter ordering) {
		this.comparator = new ByParamComparator<>(ordering.getBagKey(), ordering.getParamKey(), ordering.isAscending());
		return this;
	}
}
