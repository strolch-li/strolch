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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class MapOfLists<T, U> {

	private Map<T, List<U>> mapOfLists;

	public MapOfLists() {
		this.mapOfLists = new HashMap<>();
	}

	public Set<T> keySet() {
		return this.mapOfLists.keySet();
	}

	public List<U> getList(T t) {
		return this.mapOfLists.get(t);
	}

	public boolean addElement(T t, U u) {
		List<U> list = this.mapOfLists.get(t);
		if (list == null) {
			list = new ArrayList<>();
			this.mapOfLists.put(t, list);
		}
		return list.add(u);
	}

	public boolean addList(T t, List<U> u) {
		List<U> list = this.mapOfLists.get(t);
		if (list == null) {
			list = new ArrayList<>();
			this.mapOfLists.put(t, list);
		}
		return list.addAll(u);
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
}
