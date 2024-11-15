/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.search.predicates.NotPredicate;

/**
 * Define the search predicate, i.e. how the where clause is evaluated, or the operator with the right hand side of the
 * where clause
 */
public interface SearchPredicate {

	/**
	 * Returns true if this predicate matches the given left hand side of the where clause
	 *
	 * @param left the left side to match
	 *
	 * @return true if the predicate matches
	 */
	boolean matches(Object left);

	/**
	 * Coerces the internal right handle side of this predicate using the given coercer. This is required to handle
	 * situations where values are not compatible, i.e. Date object and date string
	 *
	 * @param coercer the coercer to be applied to the right hand side
	 *
	 * @return the new search predicate with the coerced right hand side
	 */
	SearchPredicate coerce(ValueCoercer coercer);

	/**
	 * Negates this predicated
	 *
	 * @return a new predicate where this predicate is negated
	 */
	default SearchPredicate not() {
		return new NotPredicate(this);
	}
}
