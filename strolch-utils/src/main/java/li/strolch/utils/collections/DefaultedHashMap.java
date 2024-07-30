/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.utils.collections;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class DefaultedHashMap<K, V> extends HashMap<K, V> {

	private final V defaultValue;

	/**
	 * Constructs this {@link Map} instance to have a default value on inexistent keys
	 *
	 * @param defaultValue
	 * 		the default to return if a key is not mapped
	 */
	public DefaultedHashMap(V defaultValue) {
		this.defaultValue = defaultValue;
	}

	@Override
	public V get(Object key) {
		return getOrDefault(key, this.defaultValue);
	}
}
