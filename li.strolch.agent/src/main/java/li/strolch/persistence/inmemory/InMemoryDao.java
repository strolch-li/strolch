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
package li.strolch.persistence.inmemory;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class InMemoryDao<T extends StrolchElement> implements StrolchDao<T> {

	private Map<String, Map<String, T>> elementMap;

	public InMemoryDao() {
		this.elementMap = new HashMap<>();
	}

	@Override
	public synchronized boolean hasElement(String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return false;
		return byType.containsKey(id);
	}

	@Override
	public synchronized long querySize() {
		long size = 0;
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			size += byType.size();
		}
		return size;
	}

	@Override
	public synchronized long querySize(String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return 0;
		return byType.size();
	}

	@Override
	public synchronized Set<String> queryKeySet() {

		Set<String> keySet = new HashSet<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				keySet.add(id);
			}
		}

		return keySet;
	}

	@Override
	public synchronized Set<String> queryKeySet(String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return new HashSet<>(0);
		return new HashSet<>(byType.keySet());
	}

	@Override
	public synchronized Set<String> queryTypes() {
		return new HashSet<>(this.elementMap.keySet());
	}

	@Override
	public synchronized T queryBy(String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		return byType.get(id);
	}

	@Override
	public synchronized List<T> queryAll() {
		List<T> elements = new ArrayList<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				elements.add(byType.get(id));
			}
		}

		return elements;
	}

	@Override
	public synchronized List<T> queryAll(String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return new ArrayList<>(0);
		return new ArrayList<>(byType.values());
	}

	@Override
	public synchronized void save(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		if (byType.containsKey(element.getId())) {
			String msg = "An element already exists with the id {0}. Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getId());
			throw new StrolchPersistenceException(msg);
		}

		byType.put(element.getId(), element);
	}

	@Override
	public synchronized void saveAll(List<T> elements) {
		for (T element : elements) {
			save(element);
		}
	}

	@Override
	public synchronized void update(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		byType.put(element.getId(), element);
	}

	@Override
	public synchronized void updateAll(List<T> elements) {
		for (T element : elements) {
			update(element);
		}
	}

	@Override
	public synchronized void remove(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType != null) {
			byType.remove(element.getId());

			if (byType.isEmpty()) {
				this.elementMap.remove(element.getType());
			}
		}
	}

	@Override
	public synchronized void removeAll(List<T> elements) {
		for (T element : elements) {
			remove(element);
		}
	}

	@Override
	public synchronized long removeAll() {
		long removed = 0;

		Set<String> keySet = new HashSet<>(this.elementMap.keySet());
		for (String type : keySet) {
			Map<String, T> byType = this.elementMap.remove(type);
			removed += byType.size();
			byType.clear();
		}

		return removed;
	}

	@Override
	public synchronized long removeAllBy(String type) {
		Map<String, T> byType = this.elementMap.remove(type);
		if (byType == null)
			return 0;
		long removed = byType.size();
		byType.clear();
		return removed;
	}
}
