/*
 * Copyright 2016 Robert von Burg <eitch@eitchnet.ch>
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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class MapOfSets<T, U> {

	private Map<T, Set<U>> mapOfSets;

	public MapOfSets() {
		this.mapOfSets = new HashMap<>();
	}

	public Set<T> keySet() {
		return this.mapOfSets.keySet();
	}

	public Set<U> getSet(T t) {
		return this.mapOfSets.get(t);
	}

	public boolean addElement(T t, U u) {
		Set<U> set = this.mapOfSets.get(t);
		if (set == null) {
			set = new HashSet<>();
			this.mapOfSets.put(t, set);
		}
		return set.add(u);
	}

	public boolean addSet(T t, Set<U> u) {
		Set<U> set = this.mapOfSets.get(t);
		if (set == null) {
			set = new HashSet<>();
			this.mapOfSets.put(t, set);
		}
		return set.addAll(u);
	}

	public boolean removeElement(T t, U u) {
		Set<U> set = this.mapOfSets.get(t);
		if (set == null) {
			return false;
		}
		boolean removed = set.remove(u);
		if (set.isEmpty()) {
			this.mapOfSets.remove(t);
		}

		return removed;
	}

	public Set<U> removeSet(T t) {
		return this.mapOfSets.remove(t);
	}

	public void clear() {
		Set<Entry<T, Set<U>>> entrySet = this.mapOfSets.entrySet();
		Iterator<Entry<T, Set<U>>> iter = entrySet.iterator();
		while (iter.hasNext()) {
			iter.next().getValue().clear();
			iter.remove();
		}
	}

	public boolean containsSet(T t) {
		return this.mapOfSets.containsKey(t);
	}

	public boolean containsElement(T t, U u) {
		Set<U> set = this.mapOfSets.get(t);
		if (set == null)
			return false;
		return set.contains(u);
	}

	public int sizeKeys() {
		return this.mapOfSets.size();
	}

	public int size() {
		int size = 0;
		Set<Entry<T, Set<U>>> entrySet = this.mapOfSets.entrySet();
		Iterator<Entry<T, Set<U>>> iter = entrySet.iterator();
		while (iter.hasNext()) {
			size += iter.next().getValue().size();
		}
		return size;
	}

	public int size(T t) {
		Set<U> set = this.mapOfSets.get(t);
		if (set.size() == 0)
			return 0;
		return set.size();
	}

	public boolean isEmpty() {
		return this.mapOfSets.isEmpty();
	}
}
