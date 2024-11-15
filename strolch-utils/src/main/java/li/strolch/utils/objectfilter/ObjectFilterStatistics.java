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

package li.strolch.utils.objectfilter;

import java.util.HashMap;
import java.util.Map;

public record ObjectFilterStatistics(Map<String, Integer> added, Map<String, Integer> updated,
									 Map<String, Integer> removed) {

	public ObjectFilterStatistics merge(ObjectFilterStatistics other) {
		Map<String, Integer> added = new HashMap<>(added());
		Map<String, Integer> updated = new HashMap<>(updated());
		Map<String, Integer> removed = new HashMap<>(removed());
		other.added().forEach((key, count) -> addCountKey(added, key, count));
		other.updated().forEach((key, count) -> addCountKey(updated, key, count));
		other.removed().forEach((key, count) -> addCountKey(removed, key, count));
		return new ObjectFilterStatistics(added, updated, removed);
	}

	static void addCountKey(Map<String, Integer> map, String key, int count) {
		if (map.containsKey(key))
			map.put(key, map.get(key) + count);
		else
			map.put(key, count);
	}

	@Override
	public String toString() {
		return "ObjectFilterStatistics{" + "added=" + added + ", updated=" + updated + ", removed=" + removed + '}';
	}
}
