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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.agent.api.ElementMap;
import li.strolch.model.StrolchElement;
import li.strolch.persistence.api.StrolchDao;
import li.strolch.persistence.api.StrolchPersistenceException;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.StrolchConstants;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class CachedElementMap<T extends StrolchElement> implements ElementMap<T> {

	private static final Logger logger = LoggerFactory.getLogger(CachedElementMap.class);
	private Set<String> allKeys;
	private Map<String, Map<String, T>> elementMap;

	public CachedElementMap() {
		this.allKeys = Collections.synchronizedSet(new HashSet<String>());
		this.elementMap = Collections.synchronizedMap(new HashMap<String, Map<String, T>>());
	}

	protected abstract StrolchDao<T> getDao(StrolchTransaction tx);

	@Override
	public boolean hasType(StrolchTransaction tx, String type) {
		return this.elementMap.containsKey(type);
	}

	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return false;

		return byType.containsKey(id);
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return getAllKeys(tx).size();
	}

	@Override
	public long querySize(StrolchTransaction tx, String type) {
		return getKeysBy(tx, type).size();
	}

	@Override
	public T getTemplate(StrolchTransaction tx, String type) {
		return getBy(tx, StrolchConstants.TEMPLATE, type);
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return null;

		return byType.get(id);
	}

	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		List<T> allElements = new ArrayList<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			allElements.addAll(byType.values());
		}

		return allElements;
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return Collections.emptyList();
		return new ArrayList<>(byType.values());
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return new HashSet<>(this.elementMap.keySet());
	}

	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		Set<String> keys = new HashSet<>();
		for (String type : this.elementMap.keySet()) {
			Map<String, T> byType = this.elementMap.get(type);
			keys.addAll(byType.keySet());
		}
		return keys;
	}

	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		Map<String, T> byType = this.elementMap.get(type);
		if (byType == null)
			return Collections.emptySet();
		return new HashSet<>(byType.keySet());
	}

	@Override
	public void add(StrolchTransaction tx, T element) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Element may not be null!", element); //$NON-NLS-1$

		insert(element, tx);
	}

	@Override
	public T update(StrolchTransaction tx, T element) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Element may not be null!", element); //$NON-NLS-1$

		String msg = "The element {0} can not be updated as it does not exist!"; //$NON-NLS-1$
		Map<String, T> byType = this.elementMap.get(element.getType());
		if (byType == null) {
			msg = MessageFormat.format(msg, element.getLocator());
			throw new StrolchPersistenceException(msg);
		}

		T replacedElement;
		synchronized (byType) {
			if (byType.remove(element.getId()) == null) {
				msg = MessageFormat.format(msg, element.getLocator());
				throw new StrolchPersistenceException(msg);
			}
			replacedElement = byType.put(element.getId(), element);
			getDao(tx).update(element);
		}

		return replacedElement;
	}

	@Override
	public void remove(StrolchTransaction tx, T element) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Element may not be null!", element); //$NON-NLS-1$

		String msg = "The element {0} can not be removed as it does not exist!"; //$NON-NLS-1$

		synchronized (this.elementMap) {
			Map<String, T> byType = this.elementMap.get(element.getType());
			if (byType == null) {
				msg = MessageFormat.format(msg, element.getLocator());
				throw new StrolchPersistenceException(msg);
			}

			synchronized (byType) {
				if (byType.remove(element.getId()) == null) {
					msg = MessageFormat.format(msg, element.getLocator());
					throw new StrolchPersistenceException(msg);
				}
				if (byType.isEmpty()) {
					synchronized (this.elementMap) {
						if (byType.isEmpty())
							this.elementMap.remove(element.getType());
					}
				}
				this.allKeys.remove(element.getId());
				getDao(tx).remove(element);
			}
		}
	}

	/**
	 * Special method used when starting the container to cache the values. Not to be used anywhere else but from the
	 * {@link CachedRealm} and of course through the {@link #add(StrolchTransaction, StrolchElement)}-call to not
	 * duplicate code
	 * 
	 * @param element
	 * @param tx
	 */
	void insert(T element, StrolchTransaction tx) {
		if (this.allKeys.contains(element.getId())) {
			String msg = "An element already exists with the id " + element.getId() //$NON-NLS-1$
					+ ". Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
			throw new StrolchPersistenceException(msg);
		}

		Map<String, T> byType;
		synchronized (this.elementMap) {
			byType = this.elementMap.get(element.getType());
			if (byType == null) {
				byType = Collections.synchronizedMap(new HashMap<String, T>());
				this.elementMap.put(element.getType(), byType);
			}
		}

		synchronized (byType) {
			if (byType.containsKey(element.getId())) {
				String msg = MessageFormat.format("The element {0} already exists!", element.getLocator()); //$NON-NLS-1$
				throw new StrolchPersistenceException(msg);
			}

			byType.put(element.getId(), element);
			this.allKeys.add(element.getId());
			if (tx != null)
				getDao(tx).save(element);
		}
	}

	private Map<String, List<T>> sortElementsToType(List<T> elements) {
		Map<String, List<T>> map = new HashMap<>(1);
		for (T element : elements) {
			List<T> byType = map.get(element.getType());
			if (byType == null) {
				byType = new ArrayList<>(1);
				map.put(element.getType(), byType);
			}
			byType.add(element);
		}
		return map;
	}

	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Elements may not be null!", elements); //$NON-NLS-1$

		if (elements.isEmpty())
			return;

		// sort elements by type
		Map<String, List<T>> map = sortElementsToType(elements);

		// now add elements by type
		for (String type : map.keySet()) {

			Map<String, T> byType;
			synchronized (this.elementMap) {
				byType = this.elementMap.get(type);
				if (byType == null) {
					byType = new HashMap<>();
					this.elementMap.put(type, byType);
				}

				List<T> newByType = map.get(type);
				for (T element : newByType) {
					synchronized (byType) {
						if (byType.containsKey(element.getId())) {
							String msg = "An element already exists with the id {0}. Elements of the same class must always have a unique id, regardless of their type!"; //$NON-NLS-1$
							msg = MessageFormat.format(msg, element.getId());
							throw new StrolchPersistenceException(msg);
						}

						byType.put(element.getId(), element);
						this.allKeys.add(element.getId());
					}
				}
			}
		}

		// last is to perform DB changes
		getDao(tx).saveAll(elements);
	}

	@Override
	public List<T> updateAll(StrolchTransaction tx, List<T> elements) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Elements may not be null!", elements); //$NON-NLS-1$

		if (elements.isEmpty())
			return Collections.emptyList();

		// sort elements by type
		Map<String, List<T>> map = sortElementsToType(elements);

		String msg = "The element {0} can not be updated as it does not exist!"; //$NON-NLS-1$

		List<T> replacedElements = new ArrayList<>(elements.size());

		// update elements
		for (String type : map.keySet()) {
			List<T> list = map.get(type);

			synchronized (this.elementMap) {
				Map<String, T> byType = this.elementMap.get(type);
				if (byType == null) {
					msg = MessageFormat.format(msg, list.get(0).getLocator());
					throw new StrolchPersistenceException(msg);
				}

				synchronized (byType) {
					for (T element : list) {
						T replacedElement = byType.remove(element.getId());
						if (replacedElement == null) {
							msg = MessageFormat.format(msg, element.getLocator());
							throw new StrolchPersistenceException(msg);
						}
						byType.put(element.getId(), element);
						replacedElements.add(replacedElement);
					}
				}
			}
		}

		// last is to perform DB changes
		getDao(tx).updateAll(elements);

		return replacedElements;
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) {
		DBC.PRE.assertNotNull("Transaction may not be null!", tx); //$NON-NLS-1$
		DBC.PRE.assertNotNull("Elements may not be null!", elements); //$NON-NLS-1$

		if (elements.isEmpty())
			return;

		// sort elements by type
		Map<String, List<T>> map = sortElementsToType(elements);

		String msg = "The element {0} can not be removed as it does not exist!"; //$NON-NLS-1$

		// update elements
		for (String type : map.keySet()) {
			List<T> list = map.get(type);

			synchronized (this.elementMap) {
				Map<String, T> byType = this.elementMap.get(type);
				if (byType == null) {
					msg = MessageFormat.format(msg, list.get(0).getLocator());
					throw new StrolchPersistenceException(msg);
				}
				synchronized (byType) {
					for (T element : list) {
						if (byType.remove(element.getId()) == null) {
							msg = MessageFormat.format(msg, element.getLocator());
							throw new StrolchPersistenceException(msg);
						}
						this.allKeys.remove(element.getId());

						if (byType.isEmpty()) {
							synchronized (this.elementMap) {
								if (byType.isEmpty())
									this.elementMap.remove(type);
							}
						}
					}
				}
			}
		}

		// last is to perform DB changes
		getDao(tx).removeAll(elements);
	}

	@Override
	public long removeAll(StrolchTransaction tx) {

		if (this.elementMap.isEmpty())
			return 0;

		long removed = 0;

		synchronized (this.elementMap) {
			Set<String> types = this.elementMap.keySet();
			for (String type : types) {

				Map<String, T> byType = this.elementMap.get(type);
				removed += byType.size();
				byType.clear();
			}
		}

		// last is to perform DB changes
		long daoRemoved = getDao(tx).removeAll();

		if (removed != daoRemoved) {
			String msg = "Removed {0} elements from cached map, but dao removed {1} elements!";
			logger.error(MessageFormat.format(msg, removed, daoRemoved));
		}

		return removed;
	}

	@Override
	public long removeAllBy(StrolchTransaction tx, String type) {

		long removed = 0;
		String msg = "The elements with type {0} can not be removed as they do not exist!"; //$NON-NLS-1$

		synchronized (this.elementMap) {
			Map<String, T> byType = this.elementMap.get(type);
			if (byType == null) {
				msg = MessageFormat.format(msg, type);
				throw new StrolchPersistenceException(msg);
			}

			removed = byType.size();
			byType.clear();
		}

		// last is to perform DB changes
		long daoRemoved = getDao(tx).removeAllBy(type);

		if (removed != daoRemoved) {
			msg = "Removed {0} elements from cached map for type {1}, but dao removed {3} elements!";
			logger.error(MessageFormat.format(msg, removed, type, daoRemoved));
		}

		return removed;
	}
}
