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

import li.strolch.model.Resource;

import java.util.stream.Stream;

/**
 * Performs a search for {@link Resource} elements
 */
public class ResourceSearch extends StrolchSearch<Resource, ResourceSearchResult> {

	private SearchNavigator<Resource> navigator;

	@Override
	protected SearchNavigator<Resource> getNavigator() {
		return this.navigator;
	}

	@Override
	public ResourceSearch types(String... types) {
		this.navigator = tx -> {
			Stream<Resource> cachedStream = tx.streamCachedResources(types);
			Stream<Resource> stream = tx
					.streamResources(types)
					.filter(e -> !tx.isResourceCached(e.getType(), e.getId()));
			return Stream.concat(cachedStream, stream);
		};
		return this;
	}

	@Override
	public ResourceSearch where(SearchExpression<Resource> expression) {
		super.where(expression);
		return this;
	}

	@Override
	public ResourceSearch internal() {
		super.internal();
		return this;
	}

	@Override
	protected ResourceSearchResult evaluateResult(Stream<Resource> stream) {
		return new ResourceSearchResult(stream);
	}
}
