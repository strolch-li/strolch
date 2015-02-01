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
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.ElementMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class CachedElementMap<T extends StrolchRootElement> implements ElementMap<T> {

	private static final Logger logger = LoggerFactory.getLogger(CachedElementMap.class);

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
		return getBy(tx, StrolchConstants.TEMPLATE, type);
	}

	@Override
	public synchronized T getBy(StrolchTransaction tx, String type, String id) {
		return getCachedDao().queryBy(type, id);
	}

	protected abstract void assertIsRefParam(Parameter<?> refP);

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		assertIsRefParam(refP);
		String type = refP.getUom();
		String id = refP.getValue();
		T element = getBy(tx, type, id);
		if (assertExists && element == null) {
			String msg = "The element for refP {0} with id {1} does not exist!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, refP.getLocator(), id);
			throw new StrolchException(msg);
		}
		return element;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists) throws StrolchException {
		assertIsRefParam(refP);

		List<T> elements = new ArrayList<>();
		String type = refP.getUom();
		List<String> ids = refP.getValue();

		for (String id : ids) {
			T element = getBy(tx, type, id);
			if (element != null) {
				elements.add(element);
			} else if (assertExists) {
				if (assertExists && element == null) {
					String msg = "The element for refP {0} with id {1} does not exist!"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, refP.getLocator(), id);
					throw new StrolchException(msg);
				}
			}
		}

		return elements;
	}

	@Override
	public synchronized List<T> getAllElements(StrolchTransaction tx) {
		return getCachedDao().queryAll();
	}

	@Override
	public synchronized List<T> getElementsBy(StrolchTransaction tx, String type) {
		return getCachedDao().queryAll(type);
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

	@Override
	public synchronized void add(StrolchTransaction tx, T element) {
		// first perform cached change
		getCachedDao().save(element);
		// last is to perform DB changes
		getDbDao(tx).save(element);
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

	// TODO for update we should return the updated elements, or remove the return value

	@Override
	public synchronized T update(StrolchTransaction tx, T element) {
		// first perform cached change
		getCachedDao().update(element);
		// last is to perform DB changes
		getDbDao(tx).update(element);

		return element;
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, T element) {
		// first perform cached change
		getCachedDao().remove(element);
		getDbDao(tx).remove(element);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<T> elements) {
		// first perform cached change
		getCachedDao().saveAll(elements);
		// last is to perform DB changes
		getDbDao(tx).saveAll(elements);
	}

	@Override
	public synchronized List<T> updateAll(StrolchTransaction tx, List<T> elements) {
		// first perform cached change
		getCachedDao().updateAll(elements);
		// last is to perform DB changes
		getDbDao(tx).updateAll(elements);

		return elements;
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<T> elements) {
		// first perform cached change
		getCachedDao().removeAll(elements);
		// last is to perform DB changes
		getDbDao(tx).removeAll(elements);
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
}
