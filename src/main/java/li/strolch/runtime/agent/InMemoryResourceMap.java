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
package li.strolch.runtime.agent;

import li.strolch.model.Resource;
import li.strolch.runtime.component.ComponentContainer;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class InMemoryResourceMap extends InMemoryElementMap<Resource> implements ResourceMap {

	/**
	 * @param container
	 * @param componentName
	 */
	public InMemoryResourceMap(ComponentContainer container, String componentName) {
		super(container, componentName);
	}
}
