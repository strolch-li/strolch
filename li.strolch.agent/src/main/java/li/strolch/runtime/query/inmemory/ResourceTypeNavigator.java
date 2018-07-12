/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.runtime.query.inmemory;

import java.util.stream.Stream;

import li.strolch.agent.api.ElementMap;
import li.strolch.model.Resource;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ResourceTypeNavigator extends StrolchTypeNavigator<Resource> {

	public ResourceTypeNavigator(String type) {
		super(type);
	}

	@Override
	public <U extends ElementMap<Resource>> Stream<Resource> navigate(StrolchTransaction tx, U elementMap) {
		return tx.streamResources(this.type);
	}
}
