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

package li.strolch.agent.impl;

import li.strolch.agent.api.ElementMap;
import li.strolch.agent.api.StrolchAgent;
import li.strolch.exception.StrolchElementNotFoundException;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.Version;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import static li.strolch.model.StrolchModelConstants.TEMPLATE;

public abstract class BaseElementMap<T extends StrolchRootElement> implements ElementMap<T> {

	@Override
	public synchronized T getTemplate(StrolchTransaction tx, String type) {
		return getTemplate(tx, type, false);
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type, boolean assertExists) {

		T t = getBy(tx, TEMPLATE, type);
		if (assertExists && t == null) {
			String msg = "The template with type \"{0}\" does not exist!";
			throw new StrolchElementNotFoundException(MessageFormat.format(msg, type));
		}

		if (t == null)
			return null;

		@SuppressWarnings("unchecked") T clone = (T) t.getClone();
		clone.setId(StrolchAgent.getUniqueId());
		clone.setType(type);
		return clone;
	}

	@Override
	public synchronized T getBy(StrolchTransaction tx, String type, String id) {
		return getBy(tx, type, id, false);
	}

	protected abstract T _getBy(String type, String id);

	protected abstract void assertIsRefParam(Parameter<?> refP);

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, boolean assertExists) throws StrolchException {
		T t = _getBy(type, id);
		if (assertExists && t == null) {
			String msg = "The element with type \"{0}\" and id \"{1}\" does not exist!";
			throw new StrolchElementNotFoundException(MessageFormat.format(msg, type, id));
		}

		if (t == null)
			return null;

		if (tx.isReadOnly() && !type.equals(TEMPLATE))
			return t;

		@SuppressWarnings("unchecked") T clone = (T) t.getClone(true);
		return clone;
	}

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		assertIsRefParam(refP);
		String type = refP.getUom();
		String id = refP.getValue();
		T t = getBy(tx, type, id, false);
		if (assertExists && t == null) {
			String msg = "The element with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\"";
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
						String msg = "The element with type \"{0}\" and id \"{1}\" does not exist for param \"{2}\"";
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
		if (tx.isReadOnly())
			return stream(tx).collect(Collectors.toList());

		return stream(tx).map(t -> {
			@SuppressWarnings("unchecked") T clone = (T) t.getClone(true);
			return clone;
		}).collect(Collectors.toList());
	}

	protected List<T> _getElementsByType(StrolchTransaction tx, String type, Map<String, T> byType) {
		if (tx.isReadOnly() && !type.equals(TEMPLATE))
			return new ArrayList<>(byType.values());

		return byType.values().stream().map(t -> {
			@SuppressWarnings("unchecked") T clone = (T) t.getClone(true);
			return clone;
		}).collect(Collectors.toList());
	}

	protected abstract void internalInsert(T element);

	protected void internalAdd(StrolchTransaction tx, T element) {
		if (!element.hasVersion())
			Version.setInitialVersionFor(element, tx.getCertificate().getUsername());

		internalInsert(element);
	}

	protected abstract void internalUpdate(T element);

	@Override
	public synchronized void add(StrolchTransaction tx, T element) {
		Version.updateVersionFor(element, 0, tx.getUsername(), false);
		internalAdd(tx, element);
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
		internalUpdate(element);
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			element.setVersion(getBy(tx, element.getType(), element.getId(), true).getVersion());
			Version.updateVersionFor(element, 0, tx.getUsername(), false);
			internalUpdate(element);
		}
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<T> elements) {
		for (T element : elements) {
			remove(tx, element);
		}
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version) {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id, int version, boolean assertExists)
			throws StrolchException {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public List<T> getVersionsFor(StrolchTransaction tx, String type, String id) {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public int getLatestVersionFor(StrolchTransaction tx, String type, String id) {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, T element) throws StrolchException {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public T revertToVersion(StrolchTransaction tx, String type, String id, int version) throws StrolchException {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}

	@Override
	public T undoVersion(StrolchTransaction tx, T element) throws StrolchException {
		throw new IllegalStateException(getDataStoreMode() + " mode does not support versioning");
	}
}
