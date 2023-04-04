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

import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;

import java.util.*;
import java.util.stream.Stream;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * This {@link AuditTrail} facade registers all actions performed i.e. it registers which {@link StrolchRootElement
 * StrolchRootElements} are retrieved, created, updated and deleted.
 * </p>
 *
 * <p>
 * Privilege is validated on the <code>getBy*()</code> methods.
 * </p>
 *
 * <p>
 * In a single transaction an StrolchRootElement may be created, updated and then deleted - this implementation does not
 * "squash" such actions, but registers them separately
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AuditingElementMapFacade<T extends StrolchRootElement> implements ElementMap<T> {

	protected ElementMap<T> elementMap;
	private final boolean readOnly;

	protected Set<T> read;
	protected Set<T> created;
	protected Set<T> updated;
	protected Set<T> deleted;
	protected long deletedAll;
	protected Map<String, Long> deletedAllByType;

	protected boolean observeAccessReads;

	public AuditingElementMapFacade(ElementMap<T> elementMap, boolean readOnly, boolean observeAccessReads) {
		DBC.PRE.assertNotNull("ElementMap must be set!", elementMap);
		this.elementMap = elementMap;
		this.readOnly = readOnly;
		this.observeAccessReads = observeAccessReads;
	}

	protected ElementMap<T> getElementMap() {
		return elementMap;
	}

	/**
	 * @return the read
	 */
	public Set<T> getRead() {
		if (this.read == null)
			return emptySet();
		return this.read;
	}

	/**
	 * @return the created
	 */
	public Set<T> getCreated() {
		if (this.created == null)
			return emptySet();
		return this.created;
	}

	/**
	 * @return the updated
	 */
	public Set<T> getUpdated() {
		if (this.updated == null)
			return emptySet();
		return this.updated;
	}

	/**
	 * @return the deleted
	 */
	public Set<T> getDeleted() {
		if (this.deleted == null)
			return emptySet();
		return this.deleted;
	}

	/**
	 * @return the deletedAll
	 */
	public long getDeletedAll() {
		return this.deletedAll;
	}

	/**
	 * @return the deletedAllByType
	 */
	public Map<String, Long> getDeletedAllByType() {
		if (this.deletedAllByType == null)
			return emptyMap();
		return this.deletedAllByType;
	}

	@Override
	public boolean hasType(StrolchTransaction tx, String type) {
		return this.elementMap.hasType(tx, type);
	}

	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) {
		return this.elementMap.hasElement(tx, type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return this.elementMap.querySize(tx);
	}

	@Override
	public long querySize(StrolchTransaction tx, String type) {
		return this.elementMap.querySize(tx, type);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type) {
		T template = this.elementMap.getTemplate(tx, type);
		if (this.observeAccessReads && template != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(template);
		}
		return template;
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type, boolean assertExists) throws StrolchException {
		T template = this.elementMap.getTemplate(tx, type, assertExists);
		if (this.observeAccessReads && template != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(template);
		}
		return template;
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id) {
		T element = this.elementMap.getBy(tx, type, id);
		if (this.observeAccessReads && element != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(element);
		}
		return element;
	}

	protected abstract String getElementType();

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, boolean assertExists) throws StrolchException {
		T element = this.elementMap.getBy(tx, type, id, assertExists);
		if (this.observeAccessReads && element != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(element);
		}
		return element;
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version) {
		T element = this.elementMap.getBy(tx, type, id, version);
		if (this.observeAccessReads && element != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(element);
		}
		return element;
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version, boolean assertExists)
			throws StrolchException {
		T element = this.elementMap.getBy(tx, type, id, version, assertExists);
		if (this.observeAccessReads && element != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(element);
		}
		return element;
	}

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		T element = this.elementMap.getBy(tx, refP, assertExists);
		if (this.observeAccessReads && element != null) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(element);
		}
		return element;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists)
			throws StrolchException {
		List<T> elements = this.elementMap.getBy(tx, refP, assertExists);
		if (this.observeAccessReads && !elements.isEmpty()) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.addAll(elements);
		}
		return elements;
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		List<T> versions = this.elementMap.getVersionsFor(tx, type, id);
		if (this.observeAccessReads && !versions.isEmpty()) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.add(versions.get(versions.size() - 1));
		}
		return versions;
	}

	@Override
	public int getLatestVersionFor(StrolchTransaction tx, String type, String id) {
		return this.elementMap.getLatestVersionFor(tx, type, id);
	}

	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		List<T> elements = this.elementMap.getAllElements(tx);
		if (this.observeAccessReads && !elements.isEmpty()) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.addAll(elements);
		}
		return elements;
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		List<T> elements = this.elementMap.getElementsBy(tx, type);
		if (this.observeAccessReads && !elements.isEmpty()) {
			if (this.read == null)
				this.read = new HashSet<>();
			this.read.addAll(elements);
		}
		return elements;
	}

	@Override
	public Stream<T> stream(StrolchTransaction tx, String... types) {
		Stream<T> stream = this.elementMap.stream(tx, types);
		if (this.observeAccessReads) {
			if (this.read == null)
				this.read = new HashSet<>();
			stream = stream.peek(e -> this.read.add(e));
		}
		return stream;
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return this.elementMap.getTypes(tx);
	}

	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		return this.elementMap.getAllKeys(tx);
	}

	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return this.elementMap.getKeysBy(tx, type);
	}

	private void assertNotReadOnly() {
		DBC.PRE.assertFalse("TX is marked as read-only, can not modify elements!", this.readOnly);
	}

	@Override
	public void add(StrolchTransaction tx, T element) {
		assertNotReadOnly();
		this.elementMap.add(tx, element);
		if (this.created == null)
			this.created = new HashSet<>();
		this.created.add(element);
	}

	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) {
		assertNotReadOnly();
		this.elementMap.addAll(tx, elements);
		if (this.created == null)
			this.created = new HashSet<>();
		this.created.addAll(elements);
	}

	@Override
	public void update(StrolchTransaction tx, T element) {
		assertNotReadOnly();
		element.assertNotReadonly();
		this.elementMap.update(tx, element);
		if (this.updated == null)
			this.updated = new HashSet<>();
		this.updated.add(element);
	}

	@Override
	public void updateAll(StrolchTransaction tx, List<T> elements) {
		assertNotReadOnly();
		for (T element : elements) {
			element.assertNotReadonly();
			this.elementMap.update(tx, element);
		}
		if (this.updated == null)
			this.updated = new HashSet<>();
		this.updated.addAll(elements);
	}

	@Override
	public void remove(StrolchTransaction tx, T element) {
		assertNotReadOnly();
		this.elementMap.remove(tx, element);
		if (this.deleted == null)
			this.deleted = new HashSet<>();
		this.deleted.add(element);
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) {
		assertNotReadOnly();
		this.elementMap.removeAll(tx, elements);
		if (this.deleted == null)
			this.deleted = new HashSet<>();
		this.deleted.addAll(elements);
	}

	@Override
	public long removeAll(StrolchTransaction tx) {
		assertNotReadOnly();
		long removed = this.elementMap.removeAll(tx);
		this.deletedAll += removed;
		return removed;
	}

	@Override
	public long removeAllBy(StrolchTransaction tx, String type) {
		assertNotReadOnly();
		long removed = this.elementMap.removeAllBy(tx, type);

		if (this.deletedAllByType == null)
			this.deletedAllByType = new HashMap<>();
		Long byType = this.deletedAllByType.get(type);
		if (byType == null)
			byType = 0L;
		byType = byType + removed;
		this.deletedAllByType.put(type, byType);

		return removed;
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		assertNotReadOnly();
		T element = this.elementMap.revertToVersion(tx, type, id, version);
		if (this.updated == null)
			this.updated = new HashSet<>();
		this.updated.add(element);
		return element;
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		assertNotReadOnly();
		T revertedElement = this.elementMap.revertToVersion(tx, element);
		if (this.updated == null)
			this.updated = new HashSet<>();
		this.updated.add(revertedElement);
		return revertedElement;
	}

	@Override
	public void undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		assertNotReadOnly();
		this.elementMap.undoVersion(tx, element);
		if (element.getVersion().isFirstVersion()) {
			if (this.deleted == null)
				this.deleted = new HashSet<>();
			this.deleted.add(element);
		} else {
			if (this.updated == null)
				this.updated = new HashSet<>();
			this.updated.add(element);
		}
	}
}
