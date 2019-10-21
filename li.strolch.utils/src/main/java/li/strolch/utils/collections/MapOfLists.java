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
package li.strolch.utils.collections;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class MapOfLists<T, U> {

	private Map<T, List<U>> mapOfLists;

	public MapOfLists() {
		this.mapOfLists = new HashMap<>();
	}

	public MapOfLists(Map<T, List<U>> mapOfLists) {
		this.mapOfLists = mapOfLists;
	}

	public Set<T> keySet() {
		return this.mapOfLists.keySet();
	}

	public List<U> values() {
		List<U> values = new ArrayList<>();
		forEach((t, us) -> values.addAll(us));
		return values;
	}

	public List<U> getList(T t) {
		return this.mapOfLists.get(t);
	}

	public boolean addElement(T t, U u) {
		return this.mapOfLists.computeIfAbsent(t, k -> new ArrayList<>()).add(u);
	}

	public boolean addList(T t, List<U> u) {
		return this.mapOfLists.computeIfAbsent(t, k -> new ArrayList<>()).addAll(u);
	}

	public boolean removeElement(T t, U u) {
		List<U> list = this.mapOfLists.get(t);
		if (list == null) {
			return false;
		}
		boolean removed = list.remove(u);
		if (list.isEmpty()) {
			this.mapOfLists.remove(t);
		}

		return removed;
	}

	public List<U> removeList(T t) {
		return this.mapOfLists.remove(t);
	}

	public void clear() {
		Set<Entry<T, List<U>>> entrySet = this.mapOfLists.entrySet();
		Iterator<Entry<T, List<U>>> iter = entrySet.iterator();
		while (iter.hasNext()) {
			iter.next().getValue().clear();
			iter.remove();
		}
	}

	public boolean containsList(T t) {
		return this.mapOfLists.containsKey(t);
	}

	public boolean containsElement(T t, U u) {
		List<U> list = this.mapOfLists.get(t);
		if (list == null)
			return false;
		return list.contains(u);
	}

	public int sizeKeys() {
		return this.mapOfLists.size();
	}

	public int size() {
		int size = 0;
		Set<Entry<T, List<U>>> entrySet = this.mapOfLists.entrySet();
		Iterator<Entry<T, List<U>>> iter = entrySet.iterator();
		while (iter.hasNext()) {
			size += iter.next().getValue().size();
		}
		return size;
	}

	public int size(T t) {
		List<U> list = this.mapOfLists.get(t);
		if (list.size() == 0)
			return 0;
		return list.size();
	}

	public boolean isEmpty() {
		return this.mapOfLists.isEmpty();
	}

	public MapOfLists<T, U> addAll(MapOfLists<T, U> other) {
		for (T key : other.keySet()) {
			addList(key, other.getList(key));
		}
		return this;
	}

	public List<U> getListOrDefault(T key, List<U> defaultValue) {
		List<U> u;
		return (((u = getList(key)) != null) || containsList(key)) ? u : defaultValue;
	}

	public List<U> computeIfAbsent(T key, Function<? super T, ? extends List<U>> mappingFunction) {
		Objects.requireNonNull(mappingFunction);
		List<U> u;
		if ((u = getList(key)) == null) {
			List<U> newValue;
			if ((newValue = mappingFunction.apply(key)) != null) {
				addList(key, newValue);
				return newValue;
			}
		}

		return u;
	}

	public void forEach(BiConsumer<? super T, ? super List<U>> action) {
		Objects.requireNonNull(action);
		for (Map.Entry<T, List<U>> entry : this.mapOfLists.entrySet()) {
			T k;
			List<U> u;
			try {
				k = entry.getKey();
				u = entry.getValue();
			} catch (IllegalStateException ise) {
				// this usually means the entry is no longer in the map.
				throw new ConcurrentModificationException(ise);
			}
			action.accept(k, u);
		}
	}
}
