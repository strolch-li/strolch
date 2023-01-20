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
package li.strolch.agent.impl;

import static li.strolch.model.StrolchModelConstants.TEMPLATE;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.exception.StrolchElementNotFoundException;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class TransientElementMap<T extends StrolchRootElement> implements ElementMap<T> {

	protected static final Logger logger = LoggerFactory.getLogger(TransientElementMap.class);

	private final Map<String, Map<String, T>> elementMap;

	public TransientElementMap() {
		this.elementMap = new HashMap<>();
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
		return this.elementMap.values().stream() //
				.map(map -> map.entrySet().size()) //
				.mapToInt(Integer::valueOf) //
				.sum();
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return 0;

		return byType.entrySet().size();
	}

	@Override
	public synchronized T getTemplate(StrolchTransaction tx, String type) {
		return getTemplate(tx, type, false);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type, boolean assertExists) {

		T t = getBy(tx, TEMPLATE, type);
		if (assertExists && t == null) {
			String msg = "The template with type \"{0}\" does not exist!"; //$NON-NLS-1$
			throw new StrolchElementNotFoundException(MessageFormat.format(msg, type));
		}

		if (t == null)
			return null;

		@SuppressWarnings("unchecked")
		T clone = (T) t.getClone();
		clone.setId(StrolchAgent.getUniqueId());
		clone.setType(type);
		return clone;
	}

	@Override
	public synchronized T getBy(StrolchTransaction tx, String type, String id) {
		return getBy(tx, type, id, false);
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, boolean assertExists) throws StrolchException {

		T t = null;
		Map<String, T> byType = this.elementMap.get(type);
		if (byType != null) {
			t = byType.get(id);
		}

		if (assertExists && t == null) {
			String msg = "The element with type \"{0}\" and id \"{1}\" does not exist!"; //$NON-NLS-1$
			throw new StrolchElementNotFoundException(MessageFormat.format(msg, type, id));
		}

		if (t == null)
			return null;

		@SuppressWarnings("unchecked")
		T clone = (T) t.getClone(true);
		return clone;
	}

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		assertIsRefParam(refP);
		String type = refP.getUom();
		String id = refP.getValue();
		T t = getBy(tx, type, id, false);
		if (assertExists && t == null) {
			String msg = "The element with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\""; //$NON-NLS-1$
			throw new StrolchElementNotFoundException(MessageFormat.format(msg, type, id, refP.getLocator()));
		}
		return t;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists)
			throws StrolchException {
		assertIsRefParam(refP);

		String type = refP.getUom();
		List<String> ids = refP.getValue();

		return ids.stream() //
				.map(id -> {
					T t = getBy(tx, type, id, false);
					if (assertExists && t == null) {
						String msg = "The element with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\""; //$NON-NLS-1$
						throw new StrolchElementNotFoundException(
								MessageFormat.format(msg, type, id, refP.getLocator()));
					}
					return t;
				}) //
				.filter(Objects::nonNull) //
				.collect(Collectors.toList());
	}

	@Override
	public synchronized List<T> getAllElements(StrolchTransaction tx) {
		return this.elementMap.values().stream() //
				.flatMap(e -> e.values().stream()) //
				.map(t -> {
					@SuppressWarnings("unchecked")
					T clone = (T) t.getClone(true);
					return clone;
				}) //
				.collect(Collectors.toList());
	}

	@Override
	public synchronized List<T> getElementsBy(StrolchTransaction tx, String type) {

		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return new ArrayList<>(0);

		return byType.values().stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone(true);
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public synchronized Stream<T> stream(StrolchTransaction tx, String... types) {

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
		return this.elementMap.values().stream() //
				.flatMap(map -> map.keySet().stream()) //
				.collect(Collectors.toSet());
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
	 * @param elements
	 * 		the elements to insert
	 */
	synchronized void insertAll(List<T> elements) {
		elements.forEach(this::internalInsert);
	}

	private void internalInsert(T element) {
		Map<String, T> byType = this.elementMap.computeIfAbsent(element.getType(), k -> new HashMap<>());

		// assert no object already exists with this id
		if (byType.containsKey(element.getId())) {
			String msg = "An element already exists with the id \"{0}\". Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getId());
			throw new StrolchPersistenceException(msg);
		}

		byType.put(element.getId(), element);

		// now make read only
		element.setReadOnly();
	}

	@Override
	public synchronized void add(StrolchTransaction tx, T element) {
		Version.updateVersionFor(element, 0, tx.getUsername(), false);
		internalAdd(tx, element);
	}

	protected void internalAdd(StrolchTransaction tx, T element) {
		if (!element.hasVersion())
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		internalInsert(element);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			Version.updateVersionFor(element, 0, tx.getUsername(), false);
			internalAdd(tx, element);
		}
	}

	@Override
	public synchronized void update(StrolchTransaction tx, T element) {
		element.setVersion(getBy(tx, element.getType(), element.getId(), true).getVersion());
		Version.updateVersionFor(element, 0, tx.getUsername(), false);
		internalUpdate(tx, element);
	}

	protected void internalUpdate(StrolchTransaction tx, T element) {
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			String msg = "The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		// assert object already exists with this id
		if (!byType.containsKey(element.getId())) {
			String msg = "The element does not yet exist with the type \"{0}\" and id \"{1}\". Use add() for new objects!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getType(), element.getId());
			throw new StrolchPersistenceException(msg);
		}

		byType.put(element.getId(), element);

		// now make read only
		element.setReadOnly();
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			element.setVersion(getBy(tx, element.getType(), element.getId(), true).getVersion());
			Version.updateVersionFor(element, 0, tx.getUsername(), false);
			internalUpdate(tx, element);
		}
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

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version) {
		return getBy(tx, type, id, version, false);
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version, boolean assertExists)
			throws StrolchException {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	@Override
	public int getLatestVersionFor(StrolchTransaction tx, String type, String id) {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	@Override
	public void undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		throw new IllegalStateException("Transient mode does not support versioning");
	}

	protected abstract void assertIsRefParam(Parameter<?> refP);
}
