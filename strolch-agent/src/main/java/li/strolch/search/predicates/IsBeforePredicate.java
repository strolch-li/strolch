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

package li.strolch.search.predicates;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

/**
 * <p>Implements the date is before predicate.</p>
 *
 * <b>Note:</b> Can only be used with {@link Date} or {@link ZonedDateTime} objects
 */
public class IsBeforePredicate extends DatePredicate {
	public IsBeforePredicate(ZonedDateTime dateTime, boolean inclusive) {
		super(dateTime, inclusive);
	}

	@Override
	public boolean matches(Object left) {
		if (left instanceof Date other) {
			ZonedDateTime zdt = ZonedDateTime.ofInstant(other.toInstant(), ZoneId.systemDefault());
			if (this.inclusive && this.dateTime.isEqual(zdt))
				return true;
			return zdt.isBefore(this.dateTime);
		} else if (left instanceof ZonedDateTime zdt) {
			if (this.inclusive && this.dateTime.isEqual(zdt))
				return true;
			return zdt.isBefore(this.dateTime);
		}
		throw new IllegalStateException("Unhandled object type " + left.getClass());
	}
}
