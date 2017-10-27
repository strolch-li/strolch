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

import java.text.MessageFormat;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class CachedElementMap<T extends StrolchRootElement> implements ElementMap<T> {

	private static final Logger logger = LoggerFactory.getLogger(CachedElementMap.class);

	private StrolchRealm realm;

	public CachedElementMap(StrolchRealm realm) {
		this.realm = realm;
	}

	protected StrolchRealm getRealm() {
		return this.realm;
	}

	protected abstract StrolchDao<T> getCachedDao();

	protected abstract StrolchDao<T> getDbDao(StrolchTransaction tx);

	@Override
	public synchronized boolean hasType(StrolchTransaction tx, String type) {
		return getCachedDao().queryTypes().contains(type);
	}

	@Override
	public synchronized boolean hasElement(StrolchTransaction tx, String type, String id) {
		return getCachedDao().hasElement(type, id);
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx) {
		return getCachedDao().querySize();
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type) {
		return getCachedDao().querySize(type);
	}

	@Override
	public synchronized T getTemplate(StrolchTransaction tx, String type) {
		return getTemplate(tx, type, false);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type, boolean assertExists) {
		T t = getCachedDao().queryBy(StrolchConstants.TEMPLATE, type);
		if (assertExists && t == null) {
			String msg = "The template with type {0} does not exist!"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, type));
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
		T t = getCachedDao().queryBy(type, id);
		if (assertExists && t == null) {
			String msg = "The element with type {0} and id {1} does not exist!"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, type, id));
		}

		if (t == null)
			return null;

		// TODO cloning has its issues, as queries don't return a clone!
		@SuppressWarnings("unchecked")
		T clone = (T) t.getClone();
		clone.setVersion(t.getVersion());
		return clone;
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
			String msg = "The element with type {0} and id {1} and version {2} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, type, id, version);
			throw new StrolchException(msg);
		}
		return t;
	}

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		assertIsRefParam(refP);
		String type = refP.getUom();
		String id = refP.getValue();
		T t = getBy(tx, type, id, false);
		if (assertExists && t == null) {
			String msg = "The element with type {0} and id {1} does not exist for param {2}"; //$NON-NLS-1$
			throw new StrolchException(MessageFormat.format(msg, type, id, refP.getLocator()));
		}
		return t;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists)
			throws StrolchException {
		assertIsRefParam(refP);

		String type = refP.getUom();
		List<String> ids = refP.getValue();

		return ids.stream().map(id -> getBy(tx, type, id, assertExists)).filter(Objects::nonNull)
				.collect(Collectors.toList());
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		return getDbDao(tx).queryVersionsFor(type, id);
	}

	@Override
	public synchronized List<T> getAllElements(StrolchTransaction tx) {
		List<T> all = getCachedDao().queryAll();
		return all.stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone();
			clone.setVersion(t.getVersion());
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public synchronized List<T> getElementsBy(StrolchTransaction tx, String type) {
		List<T> all = getCachedDao().queryAll(type);
		return all.stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone();
			clone.setVersion(t.getVersion());
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public synchronized Set<String> getTypes(StrolchTransaction tx) {
		return getCachedDao().queryTypes();
	}

	@Override
	public synchronized Set<String> getAllKeys(StrolchTransaction tx) {
		return getCachedDao().queryKeySet();
	}

	@Override
	public synchronized Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return getCachedDao().queryKeySet(type);
	}

	/**
	 * Special method used when starting the container to cache the values. Not to be used anywhere else but from the
	 * {@link CachedRealm}
	 *
	 * @param element
	 * @param tx
	 */
	synchronized void insert(T element) {
		getCachedDao().save(element);
	}

	@Override
	public synchronized void add(StrolchTransaction tx, T element) {
		if (realm.isVersioningEnabled())
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
		else
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		// first perform cached change
		getCachedDao().save(element);
		// last is to perform DB changes
		getDbDao(tx).save(element);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
			else
				Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
		}

		// first perform cached change
		getCachedDao().saveAll(elements);
		// last is to perform DB changes
		getDbDao(tx).saveAll(elements);
	}

	@Override
	public synchronized void update(StrolchTransaction tx, T element) {
		if (realm.isVersioningEnabled())
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
		else
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		// first perform cached change
		getCachedDao().update(element);
		// last is to perform DB changes
		getDbDao(tx).update(element);
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<T> elements) {
		for (T t : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(t, tx.getCertificate().getUsername(), false);
			else
				Version.setInitialVersionFor(t, tx.getCertificate().getUsername());
		}

		// first perform cached change
		getCachedDao().updateAll(elements);
		// last is to perform DB changes
		getDbDao(tx).updateAll(elements);
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, T element) {
		if (realm.isVersioningEnabled())
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), true);
		else
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		if (this.realm.isVersioningEnabled()) {

			// first perform cached change
			getCachedDao().remove(element);
			// last is to perform DB changes
			getDbDao(tx).update(element);

		} else {

			// first perform cached change
			getCachedDao().remove(element);
			// last is to perform DB changes
			getDbDao(tx).remove(element);
		}
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<T> elements) {
		for (T t : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(t, tx.getCertificate().getUsername(), true);
			else
				Version.setInitialVersionFor(t, tx.getCertificate().getUsername());
		}

		if (this.realm.isVersioningEnabled()) {

			// first perform cached change
			getCachedDao().removeAll(elements);
			// last is to perform DB changes
			getDbDao(tx).updateAll(elements);

		} else {

			// first perform cached change
			getCachedDao().removeAll(elements);
			// last is to perform DB changes
			getDbDao(tx).removeAll(elements);
		}
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx) {
		// first perform cached change
		long removed = getCachedDao().removeAll();
		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAll();

		if (removed != daoRemoved) {
			String msg = "Removed {0} elements from cached map, but dao removed {1} elements!"; //$NON-NLS-1$
			logger.error(MessageFormat.format(msg, removed, daoRemoved));
		}

		return removed;
	}

	@Override
	public synchronized long removeAllBy(StrolchTransaction tx, String type) {
		// first perform cached change
		long removed = getCachedDao().removeAllBy(type);
		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAllBy(type);

		if (removed != daoRemoved) {
			String msg = "Removed {0} elements from cached map for type {1}, but dao removed {3} elements!"; //$NON-NLS-1$
			logger.error(MessageFormat.format(msg, removed, type, daoRemoved));
		}

		return removed;
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		return revertToVersion(tx, element.getType(), element.getId(), element.getVersion().getVersion());
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		if (!this.realm.isVersioningEnabled()) {
			throw new StrolchPersistenceException("Can not und a version if versioning is not enabled!");
		}

		// get the current and specified version
		T current = getBy(tx, type, id, true);
		T versionT = getBy(tx, type, id, version, true);

		// create the new version
		@SuppressWarnings("unchecked")
		T clone = (T) versionT.getClone();
		clone.setVersion(current.getVersion().next(tx.getCertificate().getUsername(), false));

		// save the new version
		getCachedDao().update(clone);
		getDbDao(tx).update(clone);

		// and return new version
		return clone;
	}

	@Override
	public void undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		if (!this.realm.isVersioningEnabled()) {
			throw new StrolchPersistenceException("Can not und a version if versioning is not enabled!");
		}

		String type = element.getType();
		String id = element.getId();

		Version elementVersion = element.getVersion();

		// make sure the given element is the latest version
		T current = getBy(tx, type, id, true);
		if (!current.getVersion().equals(elementVersion)) {
			String msg = "Can not undo the version {0} as it is not the latest!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, elementVersion);
			throw new StrolchException(msg);
		}

		if (elementVersion.isFirstVersion()) {
			getCachedDao().remove(element);
			getDbDao(tx).remove(element);
		} else {
			T previous = getBy(tx, type, id, elementVersion.getPreviousVersion(), true);
			getCachedDao().update(previous);
			getDbDao(tx).removeVersion(current);
		}
	}

	protected abstract void assertIsRefParam(Parameter<?> refP);
}
