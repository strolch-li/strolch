package li.strolch.utils.collections;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.*;
import java.util.function.*;
import java.util.stream.Stream;

public class SynchronizedCollections {

	public static <T, U> MapOfLists<T, U> synchronizedMapOfLists(MapOfLists<T, U> mapOfLists) {
		return new SynchronizedMapOfLists<T, U>(mapOfLists);
	}

	public static <T, U> MapOfSets<T, U> synchronizedMapOfSets(MapOfSets<T, U> mapOfSets) {
		return new SynchronizedMapOfSets<T, U>(mapOfSets);
	}

	public static <T, U, V> MapOfMaps<T, U, V> synchronizedMapOfMaps(MapOfMaps<T, U, V> mapOfMaps) {
		return new SynchronizedMapOfMaps<T, U, V>(mapOfMaps);
	}

	private static class SynchronizedMapOfLists<T, U> extends MapOfLists<T, U> {
		private final MapOfLists<T, U> m;
		private final Object mutex;

		private transient Set<T> keySet;

		SynchronizedMapOfLists(MapOfLists<T, U> m) {
			this.m = m;
			this.mutex = this;
		}

		@Override
		public Set<T> keySet() {
			synchronized (this.mutex) {
				if (this.keySet == null) {
					this.keySet = new SynchronizedSet<>(this.m.keySet(), this.mutex);
				}
				return this.keySet;
			}
		}

		@Override
		public List<U> values() {
			synchronized (this.mutex) {
				return this.m.values();
			}
		}

		@Override
		public List<U> getList(T t) {
			synchronized (this.mutex) {
				List<U> list = this.m.getList(t);
				if (list == null)
					return null;
				return new SynchronizedList<>(list, this.mutex);
			}
		}

		@Override
		public boolean addElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.addElement(t, u);
			}
		}

		@Override
		public boolean addList(T t, List<U> u) {
			synchronized (this.mutex) {
				return this.m.addList(t, u);
			}
		}

		@Override
		public boolean removeElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.removeElement(t, u);
			}
		}

		@Override
		public List<U> removeList(T t) {
			synchronized (this.mutex) {
				return this.m.removeList(t);
			}
		}

		@Override
		public void clear() {
			synchronized (this.mutex) {
				this.m.clear();
			}
		}

		@Override
		public boolean containsList(T t) {
			synchronized (this.mutex) {
				return this.m.containsList(t);
			}
		}

		@Override
		public boolean containsElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.containsElement(t, u);
			}
		}

		@Override
		public int sizeKeys() {
			synchronized (this.mutex) {
				return this.m.sizeKeys();
			}
		}

		@Override
		public int size() {
			synchronized (this.mutex) {
				return this.m.size();
			}
		}

		@Override
		public int size(T t) {
			synchronized (this.mutex) {
				return this.m.size(t);
			}
		}

		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {
				return this.m.isEmpty();
			}
		}

		@Override
		public MapOfLists<T, U> addAll(MapOfLists<T, U> other) {
			synchronized (this.mutex) {
				this.m.addAll(other);
			}
			return this;
		}

		@Override
		public List<U> getListOrDefault(T key, List<U> defaultValue) {
			synchronized (this.mutex) {
				return new SynchronizedList<>(this.m.getListOrDefault(key, defaultValue), this.mutex);
			}
		}

		@Override
		public List<U> computeIfAbsent(T key, Function<? super T, ? extends List<U>> mappingFunction) {
			synchronized (this.mutex) {
				return new SynchronizedList<>(this.m.computeIfAbsent(key, mappingFunction), this.mutex);
			}
		}

		@Override
		public void forEach(BiConsumer<? super T, ? super List<U>> action) {
			synchronized (this.mutex) {
				this.m.forEach(action);
			}
		}
	}

	private static class SynchronizedMapOfMaps<T, U, V> extends MapOfMaps<T, U, V> {
		private final MapOfMaps<T, U, V> m;
		private final Object mutex;

		private transient Set<T> keySet;

		SynchronizedMapOfMaps(MapOfMaps<T, U, V> m) {
			this.m = m;
			this.mutex = this;
		}

		@Override
		public Set<T> keySet() {
			synchronized (this.mutex) {
				if (this.keySet == null) {
					this.keySet = new SynchronizedSet<>(this.m.keySet(), this.mutex);
				}
				return this.keySet;
			}
		}

		@Override
		public List<V> values() {
			synchronized (mutex) {
				return m.values();
			}
		}

		@Override
		public Map<U, V> getMap(T t) {
			synchronized (this.mutex) {
				Map<U, V> map = this.m.getMap(t);
				if (map == null)
					return null;
				return new SynchronizedMap<>(map, this.mutex);
			}
		}

		@Override
		public V getElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.getElement(t, u);
			}
		}

		@Override
		public V addElement(T t, U u, V v) {
			synchronized (this.mutex) {
				return this.m.addElement(t, u, v);
			}
		}

		@Override
		public List<V> getAllElements() {
			synchronized (this.mutex) {
				return this.m.getAllElements();
			}
		}

		@Override
		public List<V> getAllElements(T t) {
			synchronized (this.mutex) {
				return this.m.getAllElements(t);
			}
		}

		@Override
		public void addMap(T t, Map<U, V> u) {
			synchronized (this.mutex) {
				this.m.addMap(t, u);
			}
		}

		@Override
		public V removeElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.removeElement(t, u);
			}
		}

		@Override
		public Map<U, V> removeMap(T t) {
			synchronized (this.mutex) {
				return this.m.removeMap(t);
			}
		}

		@Override
		public void clear() {
			synchronized (this.mutex) {
				this.m.clear();
			}
		}

		@Override
		public boolean containsMap(T t) {
			synchronized (this.mutex) {
				return this.m.containsMap(t);
			}
		}

		@Override
		public boolean containsElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.containsElement(t, u);
			}
		}

		@Override
		public int sizeKeys() {
			synchronized (this.mutex) {
				return this.m.sizeKeys();
			}
		}

		@Override
		public int size() {
			synchronized (this.mutex) {
				return this.m.size();
			}
		}

		@Override
		public int size(T t) {
			synchronized (this.mutex) {
				return this.m.size(t);
			}
		}

		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {
				return this.m.isEmpty();
			}
		}

		@Override
		public MapOfMaps<T, U, V> putAll(MapOfMaps<T, U, V> other) {
			synchronized (this.mutex) {
				this.m.putAll(other);
			}
			return this;
		}

		@Override
		public Map<U, V> getMapOrDefault(T key, Map<U, V> defaultValue) {
			synchronized (this.mutex) {
				return new SynchronizedMap<>(this.m.getMapOrDefault(key, defaultValue), this.mutex);
			}
		}

		@Override
		public Map<U, V> computeIfAbsent(T key, Function<? super T, ? extends Map<U, V>> mappingFunction) {
			synchronized (this.mutex) {
				return new SynchronizedMap<>(this.m.computeIfAbsent(key, mappingFunction), this.mutex);
			}
		}

		@Override
		public void forEach(BiConsumer<? super T, ? super Map<U, V>> action) {
			synchronized (this.mutex) {
				this.m.forEach(action);
			}
		}
	}

	private static class SynchronizedMapOfSets<T, U> extends MapOfSets<T, U> {

		private final MapOfSets<T, U> m;
		private final Object mutex;

		private transient Set<T> keySet;

		public SynchronizedMapOfSets(MapOfSets<T, U> m) {
			this.m = m;
			this.mutex = this;
		}

		@Override
		public Set<T> keySet() {
			synchronized (this.mutex) {
				if (this.keySet == null) {
					this.keySet = new SynchronizedSet<>(this.m.keySet(), this.mutex);
				}
				return this.keySet;
			}
		}

		@Override
		public List<U> values() {
			synchronized (mutex) {
				return this.m.values();
			}
		}

		@Override
		public Set<U> getSet(T t) {
			synchronized (this.mutex) {
				Set<U> set = this.m.getSet(t);
				if (set == null)
					return null;
				return new SynchronizedSet<>(set, this.mutex);
			}
		}

		@Override
		public boolean addElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.addElement(t, u);
			}
		}

		@Override
		public boolean addSet(T t, Set<U> u) {
			synchronized (this.mutex) {
				return this.m.addSet(t, u);
			}
		}

		@Override
		public boolean removeElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.removeElement(t, u);
			}
		}

		@Override
		public Set<U> removeSet(T t) {
			synchronized (this.mutex) {
				return this.m.removeSet(t);
			}
		}

		@Override
		public void clear() {
			synchronized (this.mutex) {
				this.m.clear();
			}
		}

		@Override
		public boolean containsSet(T t) {
			synchronized (this.mutex) {
				return this.m.containsSet(t);
			}
		}

		@Override
		public boolean containsElement(T t, U u) {
			synchronized (this.mutex) {
				return this.m.containsElement(t, u);
			}
		}

		@Override
		public int sizeKeys() {
			synchronized (this.mutex) {
				return this.m.sizeKeys();
			}
		}

		@Override
		public int size() {
			synchronized (this.mutex) {
				return this.m.size();
			}
		}

		@Override
		public int size(T t) {
			synchronized (this.mutex) {
				return this.m.size(t);
			}
		}

		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {
				return this.m.isEmpty();
			}
		}

		@Override
		public MapOfSets<T, U> addAll(MapOfSets<T, U> other) {
			synchronized (this.mutex) {
				this.m.addAll(other);
			}
			return this;
		}

		@Override
		public Set<U> getSetOrDefault(T key, Set<U> defaultValue) {
			synchronized (this.mutex) {
				return new SynchronizedSet<>(this.m.getSetOrDefault(key, defaultValue), this.mutex);
			}
		}

		@Override
		public Set<U> computeIfAbsent(T key, Function<? super T, ? extends Set<U>> mappingFunction) {
			synchronized (this.mutex) {
				return new SynchronizedSet<>(this.m.computeIfAbsent(key, mappingFunction), this.mutex);
			}
		}

		@Override
		public void forEach(BiConsumer<? super T, ? super Set<U>> action) {
			synchronized (this.mutex) {
				this.m.forEach(action);
			}
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			synchronized (this.mutex) {
				return this.m.equals(o);
			}
		}

		@Override
		public int hashCode() {
			synchronized (this.mutex) {
				return this.m.hashCode();
			}
		}
	}

	private static class SynchronizedCollection<E> implements Collection<E>, Serializable {
		private static final long serialVersionUID = 0L;

		final Collection<E> c;
		final Object mutex;

		SynchronizedCollection(Collection<E> c, Object mutex) {
			this.c = Objects.requireNonNull(c);
			this.mutex = Objects.requireNonNull(mutex);
		}

		@Override
		public int size() {
			synchronized (this.mutex) {
				return c.size();
			}
		}

		@Override
		public boolean isEmpty() {
			synchronized (this.mutex) {
				return c.isEmpty();
			}
		}

		@Override
		public boolean contains(Object o) {
			synchronized (this.mutex) {
				return c.contains(o);
			}
		}

		@Override
		public Object[] toArray() {
			synchronized (this.mutex) {
				return c.toArray();
			}
		}

		@Override
		public <T> T[] toArray(T[] a) {
			synchronized (this.mutex) {
				return c.toArray(a);
			}
		}

		@Override
		public Iterator<E> iterator() {
			return c.iterator();
		}

		@Override
		public boolean add(E e) {
			synchronized (this.mutex) {
				return c.add(e);
			}
		}

		@Override
		public boolean remove(Object o) {
			synchronized (this.mutex) {
				return c.remove(o);
			}
		}

		@Override
		public boolean containsAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return c.containsAll(coll);
			}
		}

		@Override
		public boolean addAll(Collection<? extends E> coll) {
			synchronized (this.mutex) {
				return c.addAll(coll);
			}
		}

		@Override
		public boolean removeAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return c.removeAll(coll);
			}
		}

		@Override
		public boolean retainAll(Collection<?> coll) {
			synchronized (this.mutex) {
				return c.retainAll(coll);
			}
		}

		@Override
		public void clear() {
			synchronized (this.mutex) {
				c.clear();
			}
		}

		@Override
		public String toString() {
			synchronized (this.mutex) {
				return c.toString();
			}
		}

		@Override
		public void forEach(Consumer<? super E> consumer) {
			synchronized (this.mutex) {
				c.forEach(consumer);
			}
		}

		@Override
		public boolean removeIf(Predicate<? super E> filter) {
			synchronized (this.mutex) {
				return c.removeIf(filter);
			}
		}

		@Override
		public Spliterator<E> spliterator() {
			return c.spliterator();
		}

		@Override
		public Stream<E> stream() {
			return c.stream();
		}

		@Override
		public Stream<E> parallelStream() {
			return c.parallelStream();
		}

		private void writeObject(ObjectOutputStream s) throws IOException {
			synchronized (this.mutex) {
				s.defaultWriteObject();
			}
		}
	}

	private static class SynchronizedList<E> extends SynchronizedCollection<E> implements List<E> {

		final List<E> list;

		SynchronizedList(List<E> list, Object mutex) {
			super(list, mutex);
			this.list = list;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			synchronized (this.mutex) {
				return list.equals(o);
			}
		}

		@Override
		public int hashCode() {
			synchronized (this.mutex) {
				return list.hashCode();
			}
		}

		@Override
		public E get(int index) {
			synchronized (this.mutex) {
				return list.get(index);
			}
		}

		@Override
		public E set(int index, E element) {
			synchronized (this.mutex) {
				return list.set(index, element);
			}
		}

		@Override
		public void add(int index, E element) {
			synchronized (this.mutex) {
				list.add(index, element);
			}
		}

		@Override
		public E remove(int index) {
			synchronized (this.mutex) {
				return list.remove(index);
			}
		}

		@Override
		public int indexOf(Object o) {
			synchronized (this.mutex) {
				return list.indexOf(o);
			}
		}

		@Override
		public int lastIndexOf(Object o) {
			synchronized (this.mutex) {
				return list.lastIndexOf(o);
			}
		}

		@Override
		public boolean addAll(int index, Collection<? extends E> c) {
			synchronized (this.mutex) {
				return list.addAll(index, c);
			}
		}

		@Override
		public ListIterator<E> listIterator() {
			return list.listIterator();
		}

		@Override
		public ListIterator<E> listIterator(int index) {
			return list.listIterator(index);
		}

		@Override
		public List<E> subList(int fromIndex, int toIndex) {
			synchronized (this.mutex) {
				return new SynchronizedList<>(list.subList(fromIndex, toIndex), this.mutex);
			}
		}

		@Override
		public void replaceAll(UnaryOperator<E> operator) {
			synchronized (this.mutex) {
				list.replaceAll(operator);
			}
		}

		@Override
		public void sort(Comparator<? super E> c) {
			synchronized (this.mutex) {
				list.sort(c);
			}
		}
	}

	private static class SynchronizedSet<E> extends SynchronizedCollection<E> implements Set<E> {

		SynchronizedSet(Set<E> s, Object mutex) {
			super(s, mutex);
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			synchronized (this.mutex) {
				return c.equals(o);
			}
		}

		@Override
		public int hashCode() {
			synchronized (this.mutex) {
				return c.hashCode();
			}
		}
	}

	private static class SynchronizedSortedSet<E> extends SynchronizedSet<E> implements SortedSet<E> {

		private final SortedSet<E> ss;

		SynchronizedSortedSet(SortedSet<E> s, Object mutex) {
			super(s, mutex);
			ss = s;
		}

		@Override
		public Comparator<? super E> comparator() {
			synchronized (this.mutex) {
				return ss.comparator();
			}
		}

		@Override
		public SortedSet<E> subSet(E fromElement, E toElement) {
			synchronized (this.mutex) {
				return new SynchronizedSortedSet<>(ss.subSet(fromElement, toElement), this.mutex);
			}
		}

		@Override
		public SortedSet<E> headSet(E toElement) {
			synchronized (this.mutex) {
				return new SynchronizedSortedSet<>(ss.headSet(toElement), this.mutex);
			}
		}

		@Override
		public SortedSet<E> tailSet(E fromElement) {
			synchronized (this.mutex) {
				return new SynchronizedSortedSet<>(ss.tailSet(fromElement), this.mutex);
			}
		}

		@Override
		public E first() {
			synchronized (this.mutex) {
				return ss.first();
			}
		}

		@Override
		public E last() {
			synchronized (this.mutex) {
				return ss.last();
			}
		}
	}

	private static class SynchronizedMap<K, V> implements Map<K, V>, Serializable {

		private final Map<K, V> m;
		final Object mutex;

		SynchronizedMap(Map<K, V> m, Object mutex) {
			this.m = m;
			this.mutex = mutex;
		}

		public int size() {
			synchronized (this.mutex) {
				return m.size();
			}
		}

		public boolean isEmpty() {
			synchronized (this.mutex) {
				return m.isEmpty();
			}
		}

		public boolean containsKey(Object key) {
			synchronized (this.mutex) {
				return m.containsKey(key);
			}
		}

		public boolean containsValue(Object value) {
			synchronized (this.mutex) {
				return m.containsValue(value);
			}
		}

		public V get(Object key) {
			synchronized (this.mutex) {
				return m.get(key);
			}
		}

		public V put(K key, V value) {
			synchronized (this.mutex) {
				return m.put(key, value);
			}
		}

		public V remove(Object key) {
			synchronized (this.mutex) {
				return m.remove(key);
			}
		}

		public void putAll(Map<? extends K, ? extends V> map) {
			synchronized (this.mutex) {
				m.putAll(map);
			}
		}

		public void clear() {
			synchronized (this.mutex) {
				m.clear();
			}
		}

		private transient Set<K> keySet;
		private transient Set<Map.Entry<K, V>> entrySet;
		private transient Collection<V> values;

		public Set<K> keySet() {
			synchronized (this.mutex) {
				if (keySet == null)
					keySet = new SynchronizedSet<>(m.keySet(), this.mutex);
				return keySet;
			}
		}

		public Set<Map.Entry<K, V>> entrySet() {
			synchronized (this.mutex) {
				if (entrySet == null)
					entrySet = new SynchronizedSet<>(m.entrySet(), this.mutex);
				return entrySet;
			}
		}

		public Collection<V> values() {
			synchronized (this.mutex) {
				if (values == null)
					values = new SynchronizedCollection<>(m.values(), this.mutex);
				return values;
			}
		}

		public boolean equals(Object o) {
			if (this == o)
				return true;
			synchronized (this.mutex) {
				return m.equals(o);
			}
		}

		public int hashCode() {
			synchronized (this.mutex) {
				return m.hashCode();
			}
		}

		public String toString() {
			synchronized (this.mutex) {
				return m.toString();
			}
		}

		@Override
		public V getOrDefault(Object k, V defaultValue) {
			synchronized (this.mutex) {
				return m.getOrDefault(k, defaultValue);
			}
		}

		@Override
		public void forEach(BiConsumer<? super K, ? super V> action) {
			synchronized (this.mutex) {
				m.forEach(action);
			}
		}

		@Override
		public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
			synchronized (this.mutex) {
				m.replaceAll(function);
			}
		}

		@Override
		public V putIfAbsent(K key, V value) {
			synchronized (this.mutex) {
				return m.putIfAbsent(key, value);
			}
		}

		@Override
		public boolean remove(Object key, Object value) {
			synchronized (this.mutex) {
				return m.remove(key, value);
			}
		}

		@Override
		public boolean replace(K key, V oldValue, V newValue) {
			synchronized (this.mutex) {
				return m.replace(key, oldValue, newValue);
			}
		}

		@Override
		public V replace(K key, V value) {
			synchronized (this.mutex) {
				return m.replace(key, value);
			}
		}

		@Override
		public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
			synchronized (this.mutex) {
				return m.computeIfAbsent(key, mappingFunction);
			}
		}

		@Override
		public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
			synchronized (this.mutex) {
				return m.computeIfPresent(key, remappingFunction);
			}
		}

		@Override
		public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
			synchronized (this.mutex) {
				return m.compute(key, remappingFunction);
			}
		}

		@Override
		public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
			synchronized (this.mutex) {
				return m.merge(key, value, remappingFunction);
			}
		}

		private void writeObject(ObjectOutputStream s) throws IOException {
			synchronized (this.mutex) {
				s.defaultWriteObject();
			}
		}
	}
}
