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

import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class MapOfSets<T, U> {

	private final boolean keepInsertionOrder;
	private final Map<T, Set<U>> mapOfSets;

	public MapOfSets() {
		this.keepInsertionOrder = false;
		this.mapOfSets = getMapOfSets();
	}

	public MapOfSets(Map<T, Set<U>> mapOfSets) {
		this.keepInsertionOrder = false;
		this.mapOfSets = mapOfSets;
	}

	public MapOfSets(boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfSets = getMapOfSets();
	}

	public MapOfSets(Map<T, Set<U>> mapOfSets, boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfSets = mapOfSets;
	}

	private HashMap<T, Set<U>> getMapOfSets() {
		if (this.keepInsertionOrder)
			return new LinkedHashMap<>();
		return new HashMap<>();
	}

	private HashSet<U> getSet() {
		if (this.keepInsertionOrder)
			return new LinkedHashSet<>();
		return new HashSet<>();
	}

	public Set<T> keySet() {
		return this.mapOfSets.keySet();
	}

	public List<U> values() {
		List<U> values = new ArrayList<>();
		forEach((t, us) -> values.addAll(us));
		return values;
	}

	public Set<U> getSet(T t) {
		return this.mapOfSets.get(t);
	}

	public boolean addElement(T t, U u) {
		return this.mapOfSets.computeIfAbsent(t, k -> getSet()).add(u);
	}

	public boolean addSet(T t, Set<U> u) {
		return this.mapOfSets.computeIfAbsent(t, k -> getSet()).addAll(u);
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
		for (Entry<T, Set<U>> tSetEntry : entrySet) {
			size += tSetEntry.getValue().size();
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

	public MapOfSets<T, U> addAll(MapOfSets<T, U> other) {
		for (T key : other.keySet()) {
			addSet(key, other.getSet(key));
		}
		return this;
	}

	public Set<U> getSetOrDefault(T key, Set<U> defaultValue) {
		Set<U> u;
		return (((u = getSet(key)) != null) || containsSet(key)) ? u : defaultValue;
	}

	public Set<U> computeIfAbsent(T key, Function<? super T, ? extends Set<U>> mappingFunction) {
		Objects.requireNonNull(mappingFunction);
		Set<U> u;
		if ((u = getSet(key)) == null) {
			Set<U> newValue;
			if ((newValue = mappingFunction.apply(key)) != null) {
				this.mapOfSets.put(key, newValue);
				return newValue;
			}
		}

		return u;
	}

	public void forEach(BiConsumer<? super T, ? super Set<U>> action) {
		Objects.requireNonNull(action);
		for (Map.Entry<T, Set<U>> entry : this.mapOfSets.entrySet()) {
			T k;
			Set<U> u;
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

	public Stream<U> streamValues() {
		return this.mapOfSets.values().stream().flatMap(Collection::stream);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;

		MapOfSets<?, ?> mapOfSets1 = (MapOfSets<?, ?>) o;
		return Objects.equals(this.mapOfSets, mapOfSets1.mapOfSets);
	}

	@Override
	public int hashCode() {
		return this.mapOfSets != null ? this.mapOfSets.hashCode() : 0;
	}
}
