/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.search;

import java.util.Collection;
import java.util.stream.Stream;

public class ValueSearch<T> implements SearchPredicates {

	private ValueSearchExpression<T> expression;

	public ValueSearch<T> where(ValueSearchExpression<T> expression) {
		if (this.expression == null)
			this.expression = expression;
		else
			this.expression = this.expression.and(expression);
		return this;
	}

	/**
	 * Performs the actual search on the given input list
	 *
	 * @return the search result
	 */
	public SearchResult<T> search(Collection<T> input) {
		return search(input.stream());
	}

	/**
	 * Performs the actual search on the given input list
	 *
	 * @return the search result
	 */
	public SearchResult<T> search(Stream<T> stream) {
		if (this.expression != null)
			stream = stream.filter(e -> this.expression.matches(e));

		return new SearchResult<>(stream);
	}
}
