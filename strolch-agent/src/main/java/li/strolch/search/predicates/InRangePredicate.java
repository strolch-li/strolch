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

package li.strolch.search.predicates;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;
import li.strolch.utils.collections.DateRange;

import java.time.ZonedDateTime;
import java.util.Date;

/**
 * <p>Implements the date in range predicate.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} or {@link ZonedDateTime} objects
 */
public class InRangePredicate implements SearchPredicate {
	private final DateRange range;

	public InRangePredicate(DateRange range) {
		this.range = range;
	}

	@Override
	public boolean matches(Object left) {
		if (left instanceof Date)
			return this.range.contains((Date) left);
		else if (left instanceof ZonedDateTime)
			return this.range.contains((ZonedDateTime) left);
		throw new IllegalStateException("Unhandled object type " + left.getClass());
	}

	@Override
	public SearchPredicate coerce(ValueCoercer coercer) {
		// nothing to coerce
		return this;
	}
}
