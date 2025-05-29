/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.agent.impl.eclipsestorage;

import li.strolch.model.StrolchRootElement;
import li.strolch.persistence.api.StrolchPersistenceException;
import org.eclipse.serializer.collections.lazy.LazyHashMap;
import org.eclipse.serializer.reference.Lazy;
import org.eclipse.serializer.reference.Referencing;
import org.eclipse.store.storage.types.StorageManager;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EclipseStorageElementRoot<T extends StrolchRootElement> {

	private final Map<String, LazyHashMap<String, Lazy<T>>> elementsByType = new HashMap<>();

	public boolean hasType(String type) {
		return this.elementsByType.containsKey(type);
	}

	public boolean hasElement(String type, String id) {
		LazyHashMap<String, Lazy<T>> byId = elementsByType.get(type);
		return byId != null && byId.containsKey(id);
	}

	public int size() {
		return this.elementsByType.values().stream().mapToInt(Map::size).sum();
	}

	public int size(String type) {
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		return byId == null ? 0 : byId.size();
	}

	public T getBy(String type, String id) {
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		return byId == null ? null : Lazy.get(byId.get(id));
	}

	public List<T> getByType(String type) {
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		return byId == null ? List.of() : byId.values().stream().map(Referencing::get).toList();
	}

	public Stream<T> stream(String... types) {
		if (types.length == 0)
			return this.elementsByType.values().stream().flatMap(map -> map.values().stream().map(Referencing::get));

		if (types.length == 1) {
			LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(types[0]);
			if (byId == null)
				return Stream.empty();
			return byId.values().stream().map(Referencing::get);
		}

		Stream<T> stream = Stream.empty();
		for (String type : types) {
			LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
			if (byId != null)
				stream = Stream.concat(stream, byId.values().stream().map(Referencing::get));
		}

		return stream;
	}

	public Set<String> getTypes() {
		return new HashSet<>(this.elementsByType.keySet());
	}

	public Set<String> getAllKeys() {
		return this.elementsByType
				.values()
				.stream()
				.flatMap(map -> map.values().stream().map(tLazy -> tLazy.get().getId()))
				.collect(Collectors.toUnmodifiableSet());
	}

	public Set<String> getKeysBy(String type) {
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		if (byId == null)
			return Set.of();
		return new HashSet<>(byId.keySet());
	}

	public void add(StorageManager storageManager, T element) {
		String type = element.getType();
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.computeIfAbsent(type, _ -> new LazyHashMap<>());

		// assert no object already exists with this id
		if (byId.containsKey(element.getId())) {
			String msg = "An element already exists with the id {0} and type {1}";
			msg = MessageFormat.format(msg, element.getId(), element.getType());
			throw new StrolchPersistenceException(msg);
		}

		// now make read only
		element.setReadOnly();

		boolean newMap = byId.isEmpty();
		Lazy<T> reference = Lazy.Reference(element);
		byId.put(element.getId(), reference);
		if (newMap) {
			storageManager.storeAll(elementsByType, byId);
		} else {
			storageManager.store(byId);
		}
	}

	public void update(StorageManager storageManager, T element) {
		String type = element.getType();
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		if (byId == null)
			throw new StrolchPersistenceException(MessageFormat.format(
					"The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!",
					element.getType(), element.getId()));

		// assert object already exists with this id
		if (!byId.containsKey(element.getId())) {
			String msg
					= "The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!";
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		// now make read only
		element.setReadOnly();

		Lazy<T> reference = Lazy.Reference(element);
		byId.put(element.getId(), reference);
		storageManager.store(byId);
	}

	public void remove(StorageManager storageManager, T element) {
		String type = element.getType();
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.get(type);
		if (byId == null)
			return;

		byId.remove(element.getId());
		if (byId.isEmpty()) {
			this.elementsByType.remove(type);
			storageManager.store(elementsByType);
			storageManager.issueFullGarbageCollection();
		}
	}

	public long removeAll(StorageManager storageManager) {
		long removed = 0;
		Set<String> keySet = new HashSet<>(this.elementsByType.keySet());
		for (String type : keySet) {
			LazyHashMap<String, Lazy<T>> byId = this.elementsByType.remove(type);
			byId.forEach((s, tLazy) -> tLazy.clear());
			removed += byId.size();
			byId.clear();
		}

		storageManager.store(elementsByType);
		storageManager.issueFullGarbageCollection();

		return removed;
	}

	public long removeAllByType(StorageManager storageManager, String type) {
		long removed = 0;
		LazyHashMap<String, Lazy<T>> byId = this.elementsByType.remove(type);
		byId.forEach((s, tLazy) -> tLazy.clear());
		removed += byId.size();
		byId.clear();

		storageManager.store(elementsByType);
		storageManager.issueFullGarbageCollection();

		return removed;
	}
}
