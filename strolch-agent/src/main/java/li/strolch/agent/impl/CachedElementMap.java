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

import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;

import java.text.MessageFormat;
import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class CachedElementMap<T extends StrolchRootElement> extends TransientElementMap<T> {

	private final StrolchRealm realm;

	public CachedElementMap(StrolchRealm realm) {
		super();
		this.realm = realm;
	}

	protected abstract StrolchDao<T> getDbDao(StrolchTransaction tx);

	@Override
	public synchronized void add(StrolchTransaction tx, T element) {
		if (this.realm.isVersioningEnabled()) {
			int latestVersion = getLatestVersionFor(tx, element.getType(), element.getId()) + 1;
			Version.updateVersionFor(element, latestVersion, tx.getUsername(), false);
		} else {
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
		}

		// first perform cached change
		super.internalAdd(tx, element);

		// last is to perform DB changes
		getDbDao(tx).save(element);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<T> elements) {

		// first perform cached change
		for (T element : elements) {
			if (this.realm.isVersioningEnabled()) {
				int latestVersion = getLatestVersionFor(tx, element.getType(), element.getId()) + 1;
				Version.updateVersionFor(element, latestVersion, tx.getUsername(), false);
			} else {
				Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
			}

			internalAdd(tx, element);
		}

		// last is to perform DB changes
		getDbDao(tx).saveAll(elements);
	}

	private void updateVersion(StrolchTransaction tx, T element, boolean deleted) {
		if (this.realm.isVersioningEnabled()) {
			if (!element.hasVersion()) {
				T current = getBy(tx, element.getType(), element.getId(), true);
				if (current.hasVersion()) {
					element.setVersion(current.getVersion());
				} else {
					int currentVersion = getLatestVersionFor(tx, element.getType(), element.getId());
					Version.updateVersionFor(element, currentVersion, tx.getUsername(), deleted);
				}
			}
			Version.updateVersionFor(element, tx.getUsername(), deleted);
		} else {
			element.setVersion(getBy(tx, element.getType(), element.getId(), true).getVersion());
			Version.updateVersionFor(element, 0, tx.getUsername(), deleted);
		}
	}

	@Override
	public synchronized void update(StrolchTransaction tx, T element) {
		updateVersion(tx, element, false);

		// first perform cached change
		super.internalUpdate(element);

		// last is to perform DB changes
		getDbDao(tx).update(element);
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<T> elements) {

		// first perform cached change
		for (T t : elements) {
			updateVersion(tx, t, false);
			internalUpdate(t);
		}

		// last is to perform DB changes
		getDbDao(tx).updateAll(elements);
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, T element) {
		updateVersion(tx, element, true);

		// first perform cached change
		super.remove(tx, element);

		// last is to perform DB changes
		if (this.realm.isVersioningEnabled()) {
			getDbDao(tx).update(element);
		} else {
			getDbDao(tx).remove(element);
		}
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<T> elements) {
		for (T t : elements) {
			updateVersion(tx, t, true);
		}

		// first perform cached change
		super.removeAll(tx, elements);

		// last is to perform DB changes
		if (this.realm.isVersioningEnabled()) {
			getDbDao(tx).updateAll(elements);
		} else {
			getDbDao(tx).removeAll(elements);
		}
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx) {

		// first perform cached change
		long removed = super.removeAll(tx);

		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAll();

		if (removed != daoRemoved) {
			logger.error("Removed {} elements from cached map, but dao removed {} elements!", removed, daoRemoved);
		}

		return removed;
	}

	@Override
	public synchronized long removeAllBy(StrolchTransaction tx, String type) {

		// first perform cached change
		long removed = super.removeAllBy(tx, type);

		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAllBy(type);

		if (removed != daoRemoved) {
			logger.error("Removed {} elements from cached map for type {}, but dao removed {} elements!", removed, type,
					daoRemoved);
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
		T t = getDbDao(tx).queryBy(type, id, version);
		if (assertExists && t == null) {
			String msg = "The element with type \"{0}\" and id \"{1}\" and version \"{2}\" does not exist!";
			msg = MessageFormat.format(msg, type, id, version);
			throw new StrolchException(msg);
		}

		return t;
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		return getDbDao(tx).queryVersionsFor(type, id);
	}

	@Override
	public int getLatestVersionFor(StrolchTransaction tx, String type, String id) {
		return getDbDao(tx).queryLatestVersionFor(type, id);
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		return revertToVersion(tx, element.getType(), element.getId(), element.getVersion().getVersion());
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		if (!this.realm.isVersioningEnabled())
			throw new StrolchPersistenceException("Can not undo a version if versioning is not enabled!");

		// get the current and specified version
		T current = getBy(tx, type, id, true);
		T versionT = getBy(tx, type, id, version, true);

		// create the new version
		@SuppressWarnings("unchecked") T clone = (T) versionT.getClone();
		clone.setVersion(current.getVersion());

		// save the new version
		update(tx, clone);

		// and return new version
		return clone;
	}

	@Override
	public T undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		if (!this.realm.isVersioningEnabled())
			throw new StrolchPersistenceException("Can not undo a version if versioning is not enabled!");

		String type = element.getType();
		String id = element.getId();

		Version elementVersion = element.getVersion();

		// make sure the given element is the latest version
		T current = getBy(tx, type, id, true);
		if (!current.getVersion().equals(elementVersion)) {
			String msg = "Can not undo the version {0} as it is not the latest!";
			msg = MessageFormat.format(msg, elementVersion);
			throw new StrolchException(msg);
		}

		if (elementVersion.isFirstVersion()) {
			super.remove(tx, element);
			getDbDao(tx).remove(element);
			return null;
		} else {
			T previous = getBy(tx, type, id, elementVersion.getPreviousVersion(), true);
			super.internalUpdate(previous);
			getDbDao(tx).removeVersion(current);
			return previous;
		}
	}

	protected abstract void assertIsRefParam(Parameter<?> refP);
}
