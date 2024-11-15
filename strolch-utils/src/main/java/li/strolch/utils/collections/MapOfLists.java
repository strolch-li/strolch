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
package li.strolch.utils.collections;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class MapOfLists<T, U> {

	private final boolean keepInsertionOrder;
	private final Map<T, List<U>> mapOfLists;

	public MapOfLists() {
		this.keepInsertionOrder = false;
		this.mapOfLists = newMapOfLists();
	}

	public MapOfLists(boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfLists = newMapOfLists();
	}

	public MapOfLists(MapOfLists<T, U> mapOfLists) {
		this.keepInsertionOrder = mapOfLists.keepInsertionOrder;
		this.mapOfLists = newMapOfLists();
		mapOfLists.forEach((t, us) -> this.mapOfLists.put(t, new ArrayList<>(us)));
	}

	public MapOfLists(Map<T, List<U>> mapOfLists) {
		this.keepInsertionOrder = false;
		this.mapOfLists = newMapOfLists();
		mapOfLists.forEach((t, us) -> this.mapOfLists.put(t, new ArrayList<>(us)));
	}

	public MapOfLists(Map<T, List<U>> mapOfLists, boolean keepInsertionOrder) {
		this.keepInsertionOrder = keepInsertionOrder;
		this.mapOfLists = newMapOfLists();
		mapOfLists.forEach((t, us) -> this.mapOfLists.put(t, new ArrayList<>(us)));
	}

	private Map<T, List<U>> newMapOfLists() {
		if (this.keepInsertionOrder)
			return new LinkedHashMap<>();
		return new HashMap<>();
	}

	public Set<T> keySet() {
		return this.mapOfLists.keySet();
	}

	public List<U> values() {
		List<U> values = new ArrayList<>();
		forEach((_, us) -> values.addAll(us));
		return values;
	}

	public List<U> getList(T t) {
		return this.mapOfLists.get(t);
	}

	public boolean addElement(T t, U u) {
		return this.mapOfLists.computeIfAbsent(t, _ -> new ArrayList<>()).add(u);
	}

	public boolean addList(T t, List<U> u) {
		return this.mapOfLists.computeIfAbsent(t, _ -> new ArrayList<>()).addAll(u);
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
		for (Entry<T, List<U>> tListEntry : entrySet) {
			size += tListEntry.getValue().size();
		}
		return size;
	}

	public int size(T t) {
		List<U> list = this.mapOfLists.get(t);
		if (list == null || list.isEmpty())
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
				this.mapOfLists.put(key, newValue);
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

	public Stream<U> streamValues() {
		return this.mapOfLists.values().stream().flatMap(Collection::stream);
	}

	public Stream<Entry<T, List<U>>> stream() {
		return this.mapOfLists.entrySet().stream();
	}

	/**
	 * Returns a read only copy of this {@link MapOfSets}
	 */
	public MapOfLists<T, U> copyOf() {
		if (this instanceof ImmutableMapOfLists<T, U>)
			return this;
		return new ImmutableMapOfLists<>(this);
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;

		MapOfLists<?, ?> that = (MapOfLists<?, ?>) o;

		return Objects.equals(this.mapOfLists, that.mapOfLists);
	}

	@Override
	public int hashCode() {
		return this.mapOfLists != null ? this.mapOfLists.hashCode() : 0;
	}

	final static class ImmutableMapOfLists<T, U> extends MapOfLists<T, U> {
		ImmutableMapOfLists(MapOfLists<T, U> mapOfLists) {
			super(getCopy(mapOfLists), mapOfLists.keepInsertionOrder);
		}

		@Override
		public boolean addElement(T t, U u) {
			throw uoe();
		}

		@Override
		public boolean addList(T t, List<U> u) {
			throw uoe();
		}

		@Override
		public boolean removeElement(T t, U u) {
			throw uoe();
		}

		@Override
		public List<U> removeList(T t) {
			throw uoe();
		}

		@Override
		public void clear() {
			throw uoe();
		}

		@Override
		public MapOfLists<T, U> addAll(MapOfLists<T, U> other) {
			throw uoe();
		}

		@Override
		public List<U> computeIfAbsent(T key, Function<? super T, ? extends List<U>> mappingFunction) {
			throw uoe();
		}

		static UnsupportedOperationException uoe() {
			return new UnsupportedOperationException();
		}

		private static <T, U> Map<T, List<U>> getCopy(MapOfLists<T, U> mapOfLists) {
			Map<T, List<U>> copy = mapOfLists.newMapOfLists();
			mapOfLists.forEach((t, us) -> copy.put(t, List.copyOf(us)));
			return Map.copyOf(copy);
		}
	}
}
