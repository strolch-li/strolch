/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.utils.collections.DateRange;

/**
 * Declares specific predicates to be performed on a search expression. I.e. defines how the where clause is evaluated
 */
public interface SearchPredicates {

	default SearchPredicate isEqualTo(Object right) {
		return PredicatesSupport.isEqualTo(right);
	}

	default SearchPredicate isNotEqualTo(Object right) {
		return PredicatesSupport.isNotEqualTo(right);
	}

	default SearchPredicate isEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isEqualToIgnoreCase(right);
	}

	default SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return PredicatesSupport.isNotEqualToIgnoreCase(right);
	}

	default SearchPredicate startsWith(Object right) {
		return PredicatesSupport.startsWith(right);
	}

	default SearchPredicate startsWithIgnoreCase(Object right) {
		return PredicatesSupport.startsWithIgnoreCase(right);
	}

	default SearchPredicate endsWith(Object right) {
		return PredicatesSupport.endsWith(right);
	}

	default SearchPredicate endsWithIgnoreCase(Object right) {
		return PredicatesSupport.endsWithIgnoreCase(right);
	}

	default SearchPredicate contains(Object right) {
		return PredicatesSupport.contains(right);
	}

	default SearchPredicate containsIgnoreCase(Object right) {
		return PredicatesSupport.containsIgnoreCase(right);
	}

	default SearchPredicate listContains(Object right) {
		return PredicatesSupport.collectionContains(right);
	}

	default SearchPredicate isIn(Object right) {
		return PredicatesSupport.isIn(right);
	}

	default SearchPredicate isIn(Object... right) {
		return PredicatesSupport.isIn(right);
	}

	default SearchPredicate isInIgnoreCase(Object right) {
		return PredicatesSupport.isInIgnoreCase(right);
	}

	default SearchPredicate isInIgnoreCase(Object... right) {
		return PredicatesSupport.isInIgnoreCase(right);
	}

	default SearchPredicate inRange(DateRange range) {
		return PredicatesSupport.inRange(range);
	}
}
