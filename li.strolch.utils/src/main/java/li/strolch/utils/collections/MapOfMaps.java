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
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * <p>
 * Collection to store a tree with a depth of 3 elements. This solves having to always write the declaration:
 * </p>
 * <pre>
 * Map&lt;String, Map&lt;String, MyObject&gt;&gt; mapOfMaps = new HashMap&lt;&gt;;
 * </pre>
 * <p>
 * As an example to persist a map of MyObject where the branches are String one would write it as follows:
 * </p>
 * <pre>
 * MapOfMaps&lt;String, String, MyObject&gt; mapOfMaps = new MapOfMaps&lt;&gt;();
 * </pre>
 *
 * @param <T>
 * 		The key to a map with U as the key and V as the value
 * @param <U>
 * 		The key to get a value (leaf)
 * @param <V>
 * 		The value stored in the tree (leaf)
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class MapOfMaps<T, U, V> {

	private final boolean keepInsertionOrder;
	private final Map<T, Map<U, V>> mapOfMaps;

	public MapOfMaps() {
		this.keepInsertionOrder = false;
		this.mapOfMaps = new HashMap<>();
	}

	public MapOfMaps(Map<T, Map<U, V>> mapOfMaps) {
		this.keepInsertionOrder = false;
		this.mapOfMaps = mapOfMaps;
	}

	public MapOfMaps(Map<T, Map<U, V>> mapOfMaps, boolean keepInsertionOrder) {
		this.mapOfMaps = mapOfMaps;
		this.keepInsertionOrder = keepInsertionOrder;
	}

	public MapOfMaps(int initialSize) {
		this.keepInsertionOrder = false;
		this.mapOfMaps = new HashMap<>(initialSize);
	}

	public MapOfMaps(boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfMaps = getMapOfMaps();
	}

	public MapOfMaps(int initialSize, boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfMaps = getMapOfMaps(initialSize);
	}

	private Map<T, Map<U, V>> getMapOfMaps() {
		if (this.keepInsertionOrder)
			return new LinkedHashMap<>();
		return new HashMap<>();
	}

	private Map<T, Map<U, V>> getMapOfMaps(int initialSize) {
		if (this.keepInsertionOrder)
			return new LinkedHashMap<>(initialSize);
		return new HashMap<>(initialSize);
	}

	private HashMap<U, V> getMap() {
		if (this.keepInsertionOrder)
			return new LinkedHashMap<>();
		return new HashMap<>();
	}

	public Set<T> keySet() {
		return this.mapOfMaps.keySet();
	}

	public List<V> values() {
		List<V> values = new ArrayList<>();
		forEach((t, us) -> values.addAll(us.values()));
		return values;
	}

	public Map<U, V> getMap(T t) {
		return this.mapOfMaps.get(t);
	}

	public V getElement(T t, U u) {
		Map<U, V> map = this.mapOfMaps.get(t);
		if (map == null)
			return null;
		return map.get(u);
	}

	public V addElement(T t, U u, V v) {
		return this.mapOfMaps.computeIfAbsent(t, k -> getMap()).put(u, v);
	}

	public List<V> getAllElements() {
		List<V> all = new ArrayList<>();
		for (Map<U, V> u : this.mapOfMaps.values()) {
			all.addAll(u.values());
		}
		return all;
	}

	public List<V> getAllElements(T t) {
		List<V> all = new ArrayList<>();
		Map<U, V> map = this.mapOfMaps.get(t);
		if (map != null) {
			all.addAll(map.values());
		}
		return all;
	}

	public void addMap(T t, Map<U, V> u) {
		this.mapOfMaps.computeIfAbsent(t, k -> getMap()).putAll(u);
	}

	public V removeElement(T t, U u) {
		Map<U, V> map = this.mapOfMaps.get(t);
		if (map == null) {
			return null;
		}
		V v = map.remove(u);
		if (map.isEmpty()) {
			this.mapOfMaps.remove(t);
		}

		return v;
	}

	public Map<U, V> removeMap(T t) {
		return this.mapOfMaps.remove(t);
	}

	public void clear() {
		Set<Entry<T, Map<U, V>>> entrySet = this.mapOfMaps.entrySet();
		Iterator<Entry<T, Map<U, V>>> iter = entrySet.iterator();
		while (iter.hasNext()) {
			iter.next().getValue().clear();
			iter.remove();
		}
	}

	public boolean containsMap(T t) {
		return this.mapOfMaps.containsKey(t);
	}

	public boolean containsElement(T t, U u) {
		Map<U, V> map = this.mapOfMaps.get(t);
		return map != null && map.containsKey(u);
	}

	public int sizeKeys() {
		return this.mapOfMaps.size();
	}

	public int size() {
		int size = 0;
		Set<Entry<T, Map<U, V>>> entrySet = this.mapOfMaps.entrySet();
		for (Entry<T, Map<U, V>> anEntrySet : entrySet) {
			size += anEntrySet.getValue().size();
		}
		return size;
	}

	public int size(T t) {
		Map<U, V> map = this.mapOfMaps.get(t);
		if (map == null)
			return 0;
		return map.size();
	}

	public boolean isEmpty() {
		return this.mapOfMaps.isEmpty();
	}

	public MapOfMaps<T, U, V> putAll(MapOfMaps<T, U, V> other) {
		for (T key : other.keySet()) {
			addMap(key, other.getMap(key));
		}
		return this;
	}

	public Map<U, V> getMapOrDefault(T key, Map<U, V> defaultValue) {
		Map<U, V> u;
		return (((u = getMap(key)) != null) || containsMap(key)) ? u : defaultValue;
	}

	public Map<U, V> computeIfAbsent(T key, Function<? super T, ? extends Map<U, V>> mappingFunction) {
		Objects.requireNonNull(mappingFunction);
		Map<U, V> u;
		if ((u = getMap(key)) == null) {
			Map<U, V> newValue;
			if ((newValue = mappingFunction.apply(key)) != null) {
				this.mapOfMaps.put(key, newValue);
				return newValue;
			}
		}

		return u;
	}

	public V computeIfAbsent(T t, U u, Supplier<V> mappingFunction) {
		Objects.requireNonNull(mappingFunction);
		Map<U, V> uvMap = this.mapOfMaps.computeIfAbsent(t, k -> getMap());
		return uvMap.computeIfAbsent(u, k -> mappingFunction.get());
	}

	public void forEach(BiConsumer<? super T, ? super Map<U, V>> action) {
		Objects.requireNonNull(action);
		for (Map.Entry<T, Map<U, V>> entry : this.mapOfMaps.entrySet()) {
			T k;
			Map<U, V> u;
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

	public Stream<V> streamValues() {
		return this.mapOfMaps.values().stream().flatMap(uvMap -> uvMap.values().stream());
	}

	public Stream<Entry<T, Map<U, V>>> stream() {
		return this.mapOfMaps.entrySet().stream();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;

		MapOfMaps<?, ?, ?> mapOfMaps1 = (MapOfMaps<?, ?, ?>) o;

		return Objects.equals(this.mapOfMaps, mapOfMaps1.mapOfMaps);
	}

	@Override
	public int hashCode() {
		return this.mapOfMaps != null ? this.mapOfMaps.hashCode() : 0;
	}
}
