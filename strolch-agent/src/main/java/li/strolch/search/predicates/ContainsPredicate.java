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

import li.strolch.utils.ObjectHelper;

/**
 * Implements the contains predicate, delegating to {@link ObjectHelper#contains(Object, Object, boolean, boolean)}
 */
public class ContainsPredicate extends AbstractSearchPredicate {
	private final boolean ignoreCase;
	private final boolean matchAny;

	public ContainsPredicate(Object right, boolean ignoreCase, boolean matchAny) {
		super(right);
		this.ignoreCase = ignoreCase;
		this.matchAny = matchAny;
	}

	@Override
	public boolean matches(Object left) {
		return ObjectHelper.contains(left, this.right, this.ignoreCase, !this.matchAny);
	}
}
