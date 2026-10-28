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

import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.model.parameter.Parameter;

import java.util.*;
import java.util.stream.Stream;

/** A sparse transaction-local overlay. Immutable committed roots and unchanged type indexes are never copied. */
abstract class TransactionElementMap<T extends StrolchRootElement> extends BaseElementMap<T> {
	private enum Kind { ADD, UPDATE, REMOVE, CLEAR, CLEAR_TYPE, UNDO_VERSION }
	private record Key(String type, String id) { }
	private record Change<T>(Kind kind, T element, String type) { }

	private final TransientElementMap<T> source;
	private final StrolchTransaction owner;
	private final ModelPublication publication;
	private final String interpretation;
	private Map<Key, T> changed = Collections.emptyMap();
	private List<Change<T>> journal = Collections.emptyList();
	private Set<String> clearedTypes = Collections.emptySet();
	private boolean cleared;
	private Map<String, Map<String, T>> preparedTypes;

	TransactionElementMap(TransientElementMap<T> source, StrolchTransaction owner,
			ModelPublication publication, String interpretation) {
		this.source = source;
		this.owner = owner;
		this.publication = publication;
		this.interpretation = interpretation;
	}

	private void initializeChanges() {
		if (this.journal.isEmpty()) {
			this.changed = new LinkedHashMap<>();
			this.journal = new ArrayList<>();
			this.clearedTypes = new HashSet<>();
		}
	}

	boolean affects(String type, String id) {
		return this.cleared || this.clearedTypes.contains(type) || this.changed.containsKey(new Key(type, id));
	}

	boolean hasChanges() { return !this.journal.isEmpty(); }
	boolean hasBulkClear() { return this.cleared || !this.clearedTypes.isEmpty(); }

	@Override
	public DataStoreMode getDataStoreMode() { return this.source.getDataStoreMode(); }

	@Override
	protected void assertIsRefParam(Parameter<?> parameter) {
		ElementMapHelpers.assertIsRefParam(this.interpretation, parameter);
	}

	@Override
	protected T _getBy(String type, String id) {
		this.publication.assertAvailable();
		if (!this.changed.isEmpty()) {
			Key key = new Key(type, id);
			if (this.changed.containsKey(key)) return this.changed.get(key);
		}
		if (this.cleared || this.clearedTypes.contains(type))
			return null;
		return this.publication.readRoot(this.interpretation, type, id, () -> this.source._getBy(type, id));
	}

	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) { return _getBy(type, id) != null; }
	@Override
	public boolean hasType(StrolchTransaction tx, String type) { return querySize(tx, type) != 0; }
	@Override
	public long querySize(StrolchTransaction tx, T element) { return querySize(tx, element.getType(), element.getId()); }
	@Override
	public long querySize(StrolchTransaction tx, String type, String id) { return hasElement(tx, type, id) ? 1 : 0; }
	@Override
	public long querySize(StrolchTransaction tx) { return size(null); }
	@Override
	public long querySize(StrolchTransaction tx, String type) { return size(type); }

	private long size(String type) {
		return this.publication.read(() -> {
			long size = committedSize(type);
			for (var entry : this.changed.entrySet()) {
				if (type == null || type.equals(entry.getKey().type()))
					size += sizeDifference(entry.getKey(), entry.getValue());
			}
			return size;
		});
	}

	private long committedSize(String type) {
		if (this.cleared) return 0;
		if (type != null)
			return this.clearedTypes.contains(type) ? 0 : this.source.querySize(this.owner, type);
		long size = this.source.querySize(this.owner);
		for (String clearedType : this.clearedTypes)
			size -= this.source.querySize(this.owner, clearedType);
		return size;
	}

	private int sizeDifference(Key key, T value) {
		boolean existed = !this.cleared && !this.clearedTypes.contains(key.type()) &&
				this.source._getBy(key.type(), key.id()) != null;
		return (value == null ? 0 : 1) - (existed ? 1 : 0);
	}

	@Override
	public Stream<T> stream(StrolchTransaction tx, String... types) {
		Stream<T> committed = this.cleared ? Stream.empty() : this.publication.read(() -> this.source.stream(tx, types));
		if (!hasChanges())
			return committed;
		Set<String> selected = new HashSet<>(Arrays.asList(types));
		Stream<T> local = this.changed.entrySet().stream()
				.filter(e -> e.getValue() != null && (selected.isEmpty() || selected.contains(e.getKey().type())))
				.map(Map.Entry::getValue);
		return Stream.concat(committed.filter(e -> !this.clearedTypes.contains(e.getType()) &&
				!this.changed.containsKey(new Key(e.getType(), e.getId()))), local);
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		return stream(tx, type).map(e -> tx.isReadOnly() ? e : cloneOf(e)).toList();
	}
	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		Set<String> types = this.cleared ? new HashSet<>() : this.publication.read(() -> this.source.getTypes(tx));
		types.removeAll(this.clearedTypes);
		this.changed.keySet().forEach(key -> types.add(key.type()));
		types.removeIf(type -> querySize(tx, type) == 0);
		return types;
	}
	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		return stream(tx).map(StrolchRootElement::getId).collect(java.util.stream.Collectors.toSet());
	}
	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return stream(tx, type).map(StrolchRootElement::getId).collect(java.util.stream.Collectors.toSet());
	}

	@SuppressWarnings("unchecked")
	private T cloneOf(T element) { return (T) element.getClone(true); }

	private void stage(Kind kind, T element) {
		if (this.owner.isRollingBack())
			return; // Command undo cannot publish or persist; the entire overlay will be discarded.
		initializeChanges();
		if (!this.owner.hasLock(element)) this.owner.lock(element);
		T current = _getBy(element.getType(), element.getId());
		if (kind == Kind.ADD && current != null)
			throw new StrolchException("Element already exists: " + element.getLocator());
		if (kind != Kind.ADD && current == null)
			throw new StrolchException("Element does not exist: " + element.getLocator());
		if (element.isReadOnly())
			element = cloneOf(element);
		if (current != null)
			element.setVersion(current.getVersion());
		if (this.owner.isVersioningEnabled() && element.hasVersion())
			Version.updateVersionFor(element, this.owner.getUsername(), kind == Kind.REMOVE);
		else
			Version.updateVersionFor(element, 0, this.owner.getUsername(), kind == Kind.REMOVE);
		element.setReadOnly();
		this.journal.add(new Change<>(kind, element, null));
		this.changed.put(new Key(element.getType(), element.getId()), kind == Kind.REMOVE ? null : element);
	}

	@Override
	public void add(StrolchTransaction tx, T element) { stage(Kind.ADD, element); }
	@Override
	public void update(StrolchTransaction tx, T element) { stage(Kind.UPDATE, element); }
	@Override
	public void remove(StrolchTransaction tx, T element) { stage(Kind.REMOVE, element); }
	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) { elements.forEach(e -> add(tx, e)); }
	@Override
	public void updateAll(StrolchTransaction tx, List<T> elements) { elements.forEach(e -> update(tx, e)); }
	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) { elements.forEach(e -> remove(tx, e)); }

	@Override
	public long removeAll(StrolchTransaction tx) {
		if (this.owner.isRollingBack()) return 0;
		initializeChanges();
		long size = querySize(tx);
		this.cleared = true;
		this.changed.clear();
		this.journal.add(new Change<>(Kind.CLEAR, null, null));
		return size;
	}
	@Override
	public long removeAllBy(StrolchTransaction tx, String type) {
		if (this.owner.isRollingBack()) return 0;
		initializeChanges();
		long size = querySize(tx, type);
		this.clearedTypes.add(type);
		this.changed.keySet().removeIf(key -> key.type().equals(type));
		this.journal.add(new Change<>(Kind.CLEAR_TYPE, null, type));
		return size;
	}

	@Override
	protected void internalInsert(T element) { throw new UnsupportedOperationException(); }
	@Override
	protected void internalUpdate(T element) { throw new UnsupportedOperationException(); }

	@SuppressWarnings("unchecked")
	private StrolchDao<T> dao() {
		return ((CachedElementMap<T>) this.source).getDbDao(this.owner);
	}

	void preparePersistence() {
		this.preparedTypes = new HashMap<>();
		for (Key key : this.changed.keySet())
			this.preparedTypes.computeIfAbsent(key.type(), _ -> new java.util.concurrent.ConcurrentHashMap<>());
		if (!(this.source instanceof CachedElementMap<?>))
			return;
		PersistencePreparation preparation = new PersistencePreparation(dao());
		for (Change<T> change : this.journal)
			preparation.apply(change);
	}

	private final class PersistencePreparation {
		private final StrolchDao<T> dao;
		private final Map<Key, Integer> versions = new HashMap<>();
		private final Set<String> purgedTypes = new HashSet<>();
		private boolean purged;

		private PersistencePreparation(StrolchDao<T> dao) { this.dao = dao; }

		private void apply(Change<T> change) {
			switch (change.kind()) {
				case CLEAR -> {
					this.dao.removeAll();
					this.purged = true;
					this.versions.clear();
				}
				case CLEAR_TYPE -> {
					this.dao.removeAllBy(change.type());
					this.purgedTypes.add(change.type());
					this.versions.keySet().removeIf(key -> key.type().equals(change.type()));
				}
				case UNDO_VERSION -> {
					T element = change.element();
					this.dao.removeVersion(element);
					this.versions.put(new Key(element.getType(), element.getId()), element.getVersion().getVersion() - 1);
				}
				default -> persist(change);
			}
		}

		private int nextVersion(Change<T> change, Key key) {
			Integer known = this.versions.get(key);
			if (known != null) return known + 1;
			if (this.purged || this.purgedTypes.contains(key.type())) return 0;
			if (change.kind() == Kind.ADD) return this.dao.queryLatestVersionFor(key.type(), key.id()) + 1;
			return change.element().getVersion().getVersion();
		}

		private void persist(Change<T> change) {
			T element = change.element();
			if (owner.isVersioningEnabled()) {
				Key key = new Key(element.getType(), element.getId());
				int version = nextVersion(change, key);
				Version.updateVersionFor(element, version, owner.getUsername(), change.kind() == Kind.REMOVE);
				this.versions.put(key, version);
			}
			switch (change.kind()) {
				case ADD -> this.dao.save(element);
				case UPDATE -> this.dao.update(element);
				case REMOVE -> remove(element);
				default -> throw new AssertionError();
			}
		}

		private void remove(T element) {
			if (owner.isVersioningEnabled()) this.dao.update(element);
			else this.dao.remove(element);
		}
	}

	void publish() {
		if (this.cleared)
			this.source.publishClear(null);
		else
			this.clearedTypes.forEach(this.source::publishClear);
		this.changed.forEach((key, value) -> this.source.publishRoot(key.type(), key.id(), value, this.preparedTypes.get(key.type())));
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version, boolean assertExists) {
		this.publication.assertAvailable();
		if (!hasChanges()) return this.source.getBy(tx, type, id, version, assertExists);
		T found = getVersionsFor(tx, type, id).stream().filter(e -> e.getVersion().getVersion() == version).findFirst().orElse(null);
		if (found == null && assertExists) throw new StrolchException("Version does not exist: " + type + "/" + id + "/" + version);
		return found;
	}
	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version) {
		return getBy(tx, type, id, version, false);
	}
	@Override
	public int getLatestVersionFor(StrolchTransaction tx, String type, String id) {
		this.publication.assertAvailable();
		if (!hasChanges()) return this.source.getLatestVersionFor(tx, type, id);
		return getVersionsFor(tx, type, id).stream().mapToInt(e -> e.getVersion().getVersion()).max().orElse(-1);
	}
	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		this.publication.assertAvailable();
		Map<Integer, T> versions = new TreeMap<>();
		for (T element : this.source.getVersionsFor(tx, type, id)) versions.put(element.getVersion().getVersion(), element);
		for (Change<T> change : this.journal)
			applyHistoryChange(versions, change, type, id);
		return new ArrayList<>(versions.values());
	}
	private void applyHistoryChange(Map<Integer, T> versions, Change<T> change, String type, String id) {
		if (change.kind() == Kind.CLEAR || change.kind() == Kind.CLEAR_TYPE && type.equals(change.type())) {
			versions.clear();
			return;
		}
		T element = change.element();
		if (element == null || !element.getType().equals(type) || !element.getId().equals(id)) return;
		int number = element.getVersion().getVersion();
		if (change.kind() == Kind.UNDO_VERSION) versions.remove(number);
		else versions.put(number, cloneOf(element));
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) {
		return revertToVersion(tx, element.getType(), element.getId(), element.getVersion().getVersion());
	}
	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) {
		if (!this.owner.isVersioningEnabled()) throw new StrolchException("Versioning is not enabled");
		T previous = getBy(tx, type, id, version, true);
		update(tx, previous);
		return previous;
	}
	@Override
	public T undoVersion(StrolchTransaction tx, T element) {
		if (this.owner.isRollingBack()) return null;
		if (!this.owner.isVersioningEnabled()) throw new StrolchException("Versioning is not enabled");
		initializeChanges();
		if (!this.owner.hasLock(element)) this.owner.lock(element);
		T current = _getBy(element.getType(), element.getId());
		if (current == null || !current.getVersion().equals(element.getVersion()))
			throw new StrolchException("Cannot undo a non-current version: " + element.getLocator());
		int version = current.getVersion().getVersion();
		T previous = version == 0 ? null : getBy(tx, element.getType(), element.getId(), version - 1, true);
		if (previous != null) previous.setReadOnly();
		this.changed.put(new Key(element.getType(), element.getId()), previous);
		this.journal.add(new Change<>(Kind.UNDO_VERSION, current, null));
		return previous;
	}
}
