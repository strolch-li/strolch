/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.agent.impl;

import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toSet;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class TransientElementMap<T extends StrolchRootElement> extends BaseElementMap<T> {

	protected static final Logger logger = LoggerFactory.getLogger(TransientElementMap.class);

	private final Map<String, Map<String, T>> elementMap;

	public TransientElementMap() {
		this.elementMap = new HashMap<>();
	}

	@Override
	public DataStoreMode getDataStoreMode() {
		return DataStoreMode.TRANSIENT;
	}

	@Override
	public synchronized boolean hasType(StrolchTransaction tx, String type) {
		return this.elementMap.containsKey(type);
	}

	@Override
	public synchronized boolean hasElement(StrolchTransaction tx, String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		return byType != null && byType.get(id) != null;
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx) {
		return this.elementMap.values().stream().map(Map::size).mapToInt(Integer::valueOf).sum();
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return 0;

		return byType.size();
	}

	@Override
	protected T _getBy(String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return null;
		return byType.get(id);
	}

	@Override
	public long querySize(StrolchTransaction tx, T element) {
		return querySize(tx, element.getType(), element.getId());
	}

	@Override
	public long querySize(StrolchTransaction tx, String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return 0L;
		return byType.containsKey(id) ? 1L : 0L;
	}

	@Override
	public synchronized List<T> getElementsBy(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return new ArrayList<>(0);

		return _getElementsByType(tx, type, byType);
	}

	@Override
	public synchronized Stream<T> stream(StrolchTransaction tx, String... types) {

		// TODO XXX update this to use concatenated streams if possible

		if (types.length == 0) {
			List<T> elements = new ArrayList<>();
			for (Map<String, T> map : this.elementMap.values()) {
				elements.addAll(map.values());
			}
			return elements.stream();
		}

		if (types.length == 1) {
			Map<String, T> byType = this.elementMap.get(types[0]);
			if (byType == null)
				return Stream.empty();

			return new ArrayList<>(byType.values()).stream();
		}

		List<T> elements = new ArrayList<>();
		for (String type : types) {
			Map<String, T> byType = this.elementMap.get(type);
			if (byType == null)
				continue;

			elements.addAll(byType.values());
		}
		return elements.stream();
	}

	@Override
	public synchronized Set<String> getTypes(StrolchTransaction tx) {
		return new HashSet<>(this.elementMap.keySet());
	}

	@Override
	public synchronized Set<String> getAllKeys(StrolchTransaction tx) {
		return this.elementMap.values().stream().flatMap(map -> map.keySet().stream()).collect(toSet());
	}

	@Override
	public synchronized Set<String> getKeysBy(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return new HashSet<>(0);
		return new HashSet<>(byType.keySet());
	}

	/**
	 * Special method used when starting the container to cache the values. Not to be used anywhere else but from the
	 * {@link CachedRealm}
	 *
	 * @param elements the elements to insert
	 */
	synchronized void insertAll(List<T> elements) {
		elements.forEach(this::internalInsert);
	}

	@Override
	protected void internalInsert(T element) {
		Map<String, T> byType = this.elementMap.computeIfAbsent(element.getType(), k -> new HashMap<>());

		// assert no object already exists with this id
		if (byType.containsKey(element.getId())) {
			String msg = "An element already exists with the id {0} and type {1}";
			msg = MessageFormat.format(msg, element.getId(), element.getType());
			throw new StrolchPersistenceException(msg);
		}

		byType.put(element.getId(), element);

		// now make read only
		element.setReadOnly();
	}

	protected void internalAdd(StrolchTransaction tx, T element) {
		if (!element.hasVersion())
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		internalInsert(element);
	}

	protected void internalUpdate(T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			String msg
					= "The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!";
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		// assert object already exists with this id
		if (!byType.containsKey(element.getId())) {
			String msg
					= "The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!";
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		byType.put(element.getId(), element);

		// now make read only
		element.setReadOnly();
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType != null) {
			byType.remove(element.getId());

			if (byType.isEmpty()) {
				this.elementMap.remove(element.getType());
			}
		}
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			Map<String, T> byType = this.elementMap.get(element.getType());
			if (byType != null) {
				byType.remove(element.getId());

				if (byType.isEmpty()) {
					this.elementMap.remove(element.getType());
				}
			}
		}
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx) {
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
	public synchronized long removeAllBy(StrolchTransaction tx, String type) {
		long removed = 0;
		Map<String, T> byType = this.elementMap.remove(type);
		if (byType != null) {
			removed = byType.size();
			byType.clear();
		}

		return removed;
	}
}
