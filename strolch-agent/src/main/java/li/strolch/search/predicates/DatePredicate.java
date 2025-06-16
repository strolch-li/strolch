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

import java.time.ZonedDateTime;
import java.util.Date;

/**
 * <p>A date predicate, concrete classes implement matching.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} elements
 */
public abstract class DatePredicate implements SearchPredicate {

	protected final ZonedDateTime dateTime;
	protected final boolean inclusive;

	public DatePredicate(ZonedDateTime dateTime, boolean inclusive) {
		this.dateTime = dateTime;
		this.inclusive = inclusive;
	}

	@Override
	public SearchPredicate coerce(ValueCoercer coercer) {
		// nothing to coerce
		return this;
	}
}
