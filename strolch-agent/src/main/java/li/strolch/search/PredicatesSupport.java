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

import li.strolch.search.predicates.*;
import li.strolch.utils.collections.DateRange;

import java.time.ZonedDateTime;

/**
 * Implements predicates to be used as static imports when writing searches
 */
public class PredicatesSupport {

	public static SearchPredicate isEmpty() {
		return new IsEmptyPredicate();
	}

	public static SearchPredicate isEqualTo(Object right) {
		return new IsEqualToPredicate(right, false);
	}

	public static SearchPredicate isEqualToIgnoreCase(Object right) {
		return new IsEqualToPredicate(right, true);
	}

	public static SearchPredicate isNotEqualTo(Object right) {
		return new IsEqualToPredicate(right, false).not();
	}

	public static SearchPredicate isNotEqualToIgnoreCase(Object right) {
		return new IsEqualToPredicate(right, true).not();
	}

	public static SearchPredicate startsWith(Object right) {
		return new StartsWithPredicate(right, false);
	}

	public static SearchPredicate startsWithIgnoreCase(Object right) {
		return new StartsWithPredicate(right, true);
	}

	public static SearchPredicate endsWith(Object right) {
		return new EndsWithPredicate(right, false);
	}

	public static SearchPredicate endsWithIgnoreCase(Object right) {
		return new EndsWithPredicate(right, true);
	}

	public static SearchPredicate contains(Object right) {
		return new ContainsPredicate(right, false, false);
	}

	public static SearchPredicate containsMatchAny(Object right) {
		return new ContainsPredicate(right, false, true);
	}

	public static SearchPredicate collectionContains(Object right) {
		return new CollectionContainsPredicate(right, false);
	}

	public static SearchPredicate containsIgnoreCase(Object right) {
		return new ContainsPredicate(right, true, false);
	}

	public static SearchPredicate containsIgnoreCaseMatchAny(Object right) {
		return new ContainsPredicate(right, true, true);
	}

	public static SearchPredicate isIn(Object right) {
		return new IsInPredicate(right, false);
	}

	public static SearchPredicate isInIgnoreCase(Object right) {
		return new IsInPredicate(right, true);
	}

	public static SearchPredicate inRange(DateRange range) {
		return new InRangePredicate(range);
	}

	public static SearchPredicate isBefore(ZonedDateTime dateTime, boolean inclusive) {
		return new IsBeforePredicate(dateTime, inclusive);
	}

	public static SearchPredicate isAfter(ZonedDateTime dateTime, boolean inclusive) {
		return new IsAfterPredicate(dateTime, inclusive);
	}
}
