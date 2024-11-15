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

package li.strolch.search.predicates;

import li.strolch.search.SearchPredicate;
import li.strolch.search.ValueCoercer;

/**
 * Abstract {@link SearchPredicate} implementing coerce method and storing the right hand side of the where clause
 */
public abstract class AbstractSearchPredicate implements SearchPredicate {

	private boolean coerced;
	protected Object right;

	public AbstractSearchPredicate(Object right) {
		this.right = right;
	}

	public AbstractSearchPredicate coerce(ValueCoercer coercer) {
		if (this.coerced)
			return this;

		this.right = coercer.coerce(this.right);
		this.coerced = true;
		return this;
	}
}
