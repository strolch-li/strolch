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
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;

public class InMemoryDao<T extends StrolchRootElement> implements StrolchDao<T> {

	private Map<String, Map<String, ArrayDeque<T>>> elementMap;
	private boolean versioningEnabled;

	public InMemoryDao(boolean versioningEnabled) {
		this.versioningEnabled = versioningEnabled;
		this.elementMap = new HashMap<>();
	}

	@Override
	public synchronized boolean hasElement(String type, String id) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return false;

		ArrayDeque<T> list = byType.get(id);
		if (list == null)
			return false;

		return !list.getLast().getVersion().isDeleted();
	}

	@Override
	public synchronized long querySize() {
		long size = 0;
		for (String type : this.elementMap.keySet()) {
			Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
			size += byType.size();
		}
		return size;
	}

	@Override
	public synchronized long querySize(String type) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return 0;
		return byType.size();
	}

	@Override
	public synchronized Set<String> queryKeySet() {
		Set<String> keySet = new HashSet<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				keySet.add(id);
			}
		}
		return keySet;
	}

	@Override
	public synchronized Set<String> queryKeySet(String type) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
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
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		ArrayDeque<T> list = byType.get(id);
		if (list == null)
			return null;

		T t = list.getLast();
		if (t.getVersion() != null && t.getVersion().isDeleted())
			return null;

		return t;
	}

	@Override
	public synchronized T queryBy(String type, String id, int version) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		ArrayDeque<T> list = byType.get(id);
		if (list == null)
			return null;

		for (T t : list) {
			if (t.getVersion().getVersion() == version)
				return t;
		}

		return null;
	}

	@Override
	public synchronized List<T> queryVersionsFor(String type, String id) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		ArrayDeque<T> list = byType.get(id);
		if (list == null)
			return new ArrayList<>();
		return new ArrayList<>(list);
	}

	@Override
	public synchronized List<T> queryAll() {
		List<T> elements = new ArrayList<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
			for (String id : byType.keySet()) {
				ArrayDeque<T> list = byType.get(id);
				T last = list.getLast();
				if (last.getVersion() == null || !last.getVersion().isDeleted())
					elements.add(last);
			}
		}

		return elements;
	}

	@Override
	public synchronized List<T> queryAll(String type) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(type);
		if (byType == null)
			return new ArrayList<>(0);

		List<T> elements = new ArrayList<>();
		for (ArrayDeque<T> list : byType.values()) {
			T last = list.getLast();
			if (last.getVersion() == null || !last.getVersion().isDeleted())
				elements.add(last);
		}

		return elements;
	}

	@Override
	public synchronized void save(T element) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			byType = new HashMap<>();
			this.elementMap.put(element.getType(), byType);
		}

		ArrayDeque<T> list = byType.get(element.getId());

		// only allow add for existing id, if the existing one is "deleted"
		if (list != null && !list.getLast().getVersion().isDeleted()) {
			String msg = "An element already exists with the id {0}. Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getId());
			throw new StrolchPersistenceException(msg);
		}

		if (list == null) {
			list = new ArrayDeque<>();
			byType.put(element.getId(), list);
		}

		list.addLast(element);
	}

	@Override
	public synchronized void saveAll(List<T> elements) {
		for (T element : elements) {
			save(element);
		}
	}

	@Override
	public synchronized void update(T element) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			String msg = "The element does not yet exist with the type {0} and id {1}. Use add() for new objects!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		ArrayDeque<T> list = byType.get(element.getId());
		if (list == null) {
			String msg = "The element does not yet exist with the type {0} and id {1}. Use add() for new objects!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		if (!this.versioningEnabled)
			list.clear();

		list.addLast(element);
	}

	@Override
	public synchronized void updateAll(List<T> elements) {
		for (T element : elements) {
			update(element);
		}
	}

	@Override
	public synchronized void remove(T element) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.get(element.getType());
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
			Map<String, ArrayDeque<T>> byType = this.elementMap.remove(type);
			removed += byType.size();
			byType.clear();
		}

		return removed;
	}

	@Override
	public synchronized long removeAllBy(String type) {
		Map<String, ArrayDeque<T>> byType = this.elementMap.remove(type);
		if (byType == null)
			return 0;
		long removed = byType.size();
		byType.clear();
		return removed;
	}

	@Override
	public synchronized void removeVersion(T element) throws StrolchPersistenceException {

		Map<String, ArrayDeque<T>> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			String msg = "The element does not yet exist with the type {0}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType());
			throw new StrolchPersistenceException(msg);
		}

		ArrayDeque<T> list = byType.get(element.getId());
		if (list == null) {
			String msg = "The element does not yet exist with the type {0} and id {1}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		T last = list.getLast();
		if (!last.getVersion().equals(element.getVersion())) {
			String msg = "The current version {0} is not the same as the version to remove {1}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, last.getVersion(), element.getVersion());
			throw new StrolchPersistenceException(msg);
		}

		list.removeLast();

		if (list.isEmpty()) {
			byType.remove(element.getId());
		}
	}
}
