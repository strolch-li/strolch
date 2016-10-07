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
 * 
 * @param <T>
 */
public abstract class TransactionalElementMap<T extends StrolchRootElement> implements ElementMap<T> {

	private StrolchRealm realm;

	public TransactionalElementMap(StrolchRealm realm) {
		this.realm = realm;
	}

	protected StrolchRealm getRealm() {
		return this.realm;
	}

	protected abstract StrolchDao<T> getDao(StrolchTransaction tx);

	@Override
	public boolean hasType(StrolchTransaction tx, String type) {
		return getDao(tx).queryTypes().contains(type);
	}

	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) {
		return getDao(tx).hasElement(type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return getDao(tx).querySize();
	}

	@Override
	public long querySize(StrolchTransaction tx, String type) {
		return getDao(tx).querySize(type);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type) {
		return getTemplate(tx, type, false);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type, boolean assertExists) throws StrolchException {
		T t = getBy(tx, StrolchConstants.TEMPLATE, type);
		if (assertExists && t == null) {
			String msg = "The template for type {0} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, type);
			throw new StrolchException(msg);
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
	public T getBy(StrolchTransaction tx, String type, String id) {
		return getBy(tx, type, id, false);
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, boolean assertExists) throws StrolchException {
		T t = getDao(tx).queryBy(type, id);
		if (assertExists && t == null) {
			String msg = "The element for type {0} and id {1} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, type, id);
			throw new StrolchException(msg);
		}

		if (t == null)
			return null;

		if (!this.realm.getMode().isTransient())
			return t;

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
		T t = getDao(tx).queryBy(type, id, version);
		if (assertExists && t == null) {
			String msg = "The element for type {0} and id {1} and version {2} does not exist!"; //$NON-NLS-1$
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
		T t = getBy(tx, type, id);
		if (assertExists && t == null) {
			String msg = "The element for refP {0} with id {1} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, refP.getLocator(), id);
			throw new StrolchException(msg);
		}

		if (t == null)
			return null;

		if (!this.realm.getMode().isTransient())
			return t;

		@SuppressWarnings("unchecked")
		T clone = (T) t.getClone();
		clone.setVersion(t.getVersion());
		return clone;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists)
			throws StrolchException {
		assertIsRefParam(refP);

		String type = refP.getUom();
		List<String> ids = refP.getValue();

		return ids.stream().map(id -> {
			T t = getBy(tx, type, id);
			if (assertExists && t == null) {
				String msg = "The element for refP {0} with id {1} does not exist!"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, refP.getLocator(), id);
				throw new StrolchException(msg);
			}
			return t;
		}).filter(Objects::nonNull).map(t -> {
			if (!this.realm.getMode().isTransient()) {
				return t;
			} else {
				@SuppressWarnings("unchecked")
				T clone = (T) t.getClone();
				clone.setVersion(t.getVersion());
				return clone;
			}
		}).collect(Collectors.toList());
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		return getDao(tx).queryVersionsFor(type, id).stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone();
			clone.setVersion(t.getVersion());
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		List<T> all = getDao(tx).queryAll();

		if (this.realm.getMode().isTransient())
			return all;

		return all.stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone();
			clone.setVersion(t.getVersion());
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		List<T> all = getDao(tx).queryAll(type);

		if (this.realm.getMode().isTransient())
			return all;

		return all.stream().map(t -> {
			@SuppressWarnings("unchecked")
			T clone = (T) t.getClone();
			clone.setVersion(t.getVersion());
			return clone;
		}).collect(Collectors.toList());
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return getDao(tx).queryTypes();
	}

	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		return getDao(tx).queryKeySet();
	}

	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return getDao(tx).queryKeySet(type);
	}

	@Override
	public void add(StrolchTransaction tx, T element) {
		if (realm.isVersioningEnabled())
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
		else
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		getDao(tx).save(element);
		getDao(tx).flush();
	}

	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
			else
				Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
		}

		getDao(tx).saveAll(elements);
		getDao(tx).flush();
	}

	@Override
	public void update(StrolchTransaction tx, T element) {
		if (realm.isVersioningEnabled())
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
		else
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		getDao(tx).update(element);
		getDao(tx).flush();
	}

	@Override
	public void updateAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(element, tx.getCertificate().getUsername(), false);
			else
				Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
		}

		getDao(tx).updateAll(elements);
		getDao(tx).flush();
	}

	@Override
	public void remove(StrolchTransaction tx, T element) {
		if (this.realm.isVersioningEnabled()) {
			Version.updateVersionFor(element, tx.getCertificate().getUsername(), true);
			getDao(tx).update(element);
		} else {
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
			getDao(tx).remove(element);
		}
		getDao(tx).flush();
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			if (realm.isVersioningEnabled())
				Version.updateVersionFor(element, tx.getCertificate().getUsername(), true);
			else
				Version.setInitialVersionFor(element, tx.getCertificate().getUsername());
		}

		if (this.realm.isVersioningEnabled()) {
			getDao(tx).updateAll(elements);
		} else {
			getDao(tx).removeAll(elements);
		}
		getDao(tx).flush();
	}

	@Override
	public long removeAll(StrolchTransaction tx) {
		long removed = getDao(tx).removeAll();
		getDao(tx).flush();
		return removed;
	}

	@Override
	public long removeAllBy(StrolchTransaction tx, String type) {
		long removed = getDao(tx).removeAllBy(type);
		getDao(tx).flush();
		return removed;
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		return revertToVersion(tx, element.getType(), element.getId(), element.getVersion().getVersion());
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		if (!this.realm.isVersioningEnabled()) {
			throw new StrolchPersistenceException("Can not revert to a version if versioning is not enabled!");
		}

		// get the current and specified version
		T current = getBy(tx, type, id, true);
		T versionT = getBy(tx, type, id, version, true);

		// create the new version
		@SuppressWarnings("unchecked")
		T clone = (T) versionT.getClone();
		clone.setVersion(current.getVersion().next(tx.getCertificate().getUsername(), false));

		// save the new version
		getDao(tx).update(clone);
		getDao(tx).flush();

		// and return new version
		return clone;
	}

	@Override
	public void undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		if (!this.realm.isVersioningEnabled()) {
			throw new StrolchPersistenceException("Can not undo a version if versioning is not enabled!");
		}

		// make sure the given element is the latest version
		T current = getBy(tx, element.getType(), element.getId(), true);
		if (!current.getVersion().equals(element.getVersion())) {
			String msg = "Can not undo the version {0} as it is not the latest {1}!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getVersion(), current.getVersion());
			throw new StrolchException(msg);
		}

		getDao(tx).removeVersion(current);
		getDao(tx).flush();
	}

	protected abstract void assertIsRefParam(Parameter<?> refP);
}
