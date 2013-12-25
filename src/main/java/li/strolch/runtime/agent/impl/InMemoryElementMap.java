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
package li.strolch.runtime.agent.impl;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.StrolchElement;
import li.strolch.runtime.agent.api.ElementMap;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public abstract class InMemoryElementMap<T extends StrolchElement> implements ElementMap<T> {

	private Map<String, Map<String, T>> elementMap;

	public InMemoryElementMap() {
		this.elementMap = new HashMap<>();
	}

	@Override
	public boolean hasElement(String type, String id) {
		return this.elementMap.containsKey(type) && this.elementMap.get(type).containsKey(id);
	}

	@Override
	public boolean hasType(String type) {
		return this.elementMap.containsKey(type);
	}

	@Override
	public T getBy(String type, String id) {
		if (StringHelper.isEmpty(type) || StringHelper.isEmpty(id))
			throw new IllegalArgumentException("type and id may not be null!"); //$NON-NLS-1$

		Map<String, T> byTypeMap = this.elementMap.get(type);
		if (byTypeMap == null || byTypeMap.isEmpty()) {
			String msg = MessageFormat.format("There is no element with the type {0} and id {1}", type, id); //$NON-NLS-1$
			throw new IllegalArgumentException(msg);
		}

		T element = byTypeMap.get(id);
		if (element == null) {
			String msg = MessageFormat.format("There is no element with the type {0} and id {1}", type, id); //$NON-NLS-1$
			throw new IllegalArgumentException(msg);
		}

		return element;
	}

	public List<T> getAllElements() {

		List<T> allElements = new ArrayList<>();
		for (Map<String, T> elementsByType : this.elementMap.values()) {
			allElements.addAll(elementsByType.values());
		}

		return allElements;
	}

	public List<T> getElementsBy(String type) {
		if (StringHelper.isEmpty(type))
			throw new IllegalArgumentException("type may not be null!"); //$NON-NLS-1$

		Map<String, T> elementsByType = this.elementMap.get(type);
		if (elementsByType == null || elementsByType.isEmpty()) {
			String msg = MessageFormat.format("There are no elements with the type {0}", type); //$NON-NLS-1$
			throw new IllegalArgumentException(msg);
		}

		return new ArrayList<>(elementsByType.values());
	}

	public Set<String> getTypes() {
		return new HashSet<>(this.elementMap.keySet());
	}

	public Set<String> getAllKeys() {

		Set<String> allKeys = new HashSet<>();
		for (Map<String, T> elementsByType : this.elementMap.values()) {
			allKeys.addAll(elementsByType.keySet());
		}

		return allKeys;
	}

	public Set<String> getKeysBy(String type) {
		if (StringHelper.isEmpty(type))
			throw new IllegalArgumentException("type may not be null!"); //$NON-NLS-1$

		Map<String, T> elementsByType = this.elementMap.get(type);
		if (elementsByType == null || elementsByType.isEmpty()) {
			String msg = MessageFormat.format("There are no elements with the type {0}", type); //$NON-NLS-1$
			throw new IllegalArgumentException(msg);
		}

		return new HashSet<>(elementsByType.keySet());
	}

	@Override
	public void add(T element) {
		if (element == null)
			throw new IllegalArgumentException("element may not be null!"); //$NON-NLS-1$

		String type = element.getType();
		Map<String, T> byTypeMap = getByTypeMap(type);

		if (byTypeMap.containsKey(element.getId())) {
			String msg = MessageFormat.format("The element already exists with the locator {0}", element.getLocator()); //$NON-NLS-1$
			throw new IllegalStateException(msg);
		}

		byTypeMap.put(element.getId(), element);
	}

	private Map<String, T> getByTypeMap(String type) {
		Map<String, T> byTypeMap = this.elementMap.get(type);
		if (byTypeMap == null) {
			byTypeMap = new HashMap<>();
			this.elementMap.put(type, byTypeMap);
		}
		return byTypeMap;
	}

	@Override
	public void update(T element) {
		if (element == null)
			throw new IllegalArgumentException("element may not be null!"); //$NON-NLS-1$

		String type = element.getType();
		Map<String, T> byTypeMap = getByTypeMap(type);
		if (!byTypeMap.containsKey(element.getId())) {
			String msg = "The element can not be updated as it does not exist in map with locator {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getLocator());
			throw new IllegalStateException(msg);
		}

		byTypeMap.remove(element.getId());
		byTypeMap.put(element.getId(), element);
	}

	@Override
	public void remove(T element) {
		if (element == null)
			throw new IllegalArgumentException("element may not be null!"); //$NON-NLS-1$

		String type = element.getType();
		Map<String, T> byTypeMap = getByTypeMap(type);
		if (!byTypeMap.containsKey(element.getId())) {
			String msg = "The element can not be removed as it does not exist in map with locator {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, element.getLocator());
			throw new IllegalStateException(msg);
		}

		byTypeMap.remove(element.getId());
	}
}
