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

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.utils.dbc.DBC;

/**
 * <p>
 * This {@link AuditTrail} facade registers all actions performed i.e. it registers which {@link StrolchRootElement
 * StrolchRootElements} are retrieved, created, updated and deleted.
 * </p>
 * 
 * <p>
 * In a single transaction an StrolchRootElement may be created, updated and then deleted - this implementation does not
 * "squash" such actions, but registers them separately
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditingElementMapFacade<T extends StrolchRootElement> implements ElementMap<T> {

	protected ElementMap<T> elementMap;

	protected Set<T> read;
	protected Set<T> created;
	protected Set<T> updated;
	protected Set<T> deleted;
	protected long deletedAll;
	protected Map<String, Long> deletedAllByType;

	protected boolean observeAccessReads;

	public AuditingElementMapFacade(ElementMap<T> elementMap, boolean observeAccessReads) {
		DBC.PRE.assertNotNull("ElementMap must be set!", elementMap); //$NON-NLS-1$
		this.elementMap = elementMap;
		this.observeAccessReads = observeAccessReads;

		this.created = new HashSet<>();
		this.read = new HashSet<>();
		this.updated = new HashSet<>();
		this.deleted = new HashSet<>();
		this.deletedAllByType = new HashMap<>();
	}

	protected ElementMap<T> getElementMap() {
		return elementMap;
	}

	/**
	 * @return the read
	 */
	public Set<T> getRead() {
		return this.read;
	}

	/**
	 * @return the created
	 */
	public Set<T> getCreated() {
		return this.created;
	}

	/**
	 * @return the updated
	 */
	public Set<T> getUpdated() {
		return this.updated;
	}

	/**
	 * @return the deleted
	 */
	public Set<T> getDeleted() {
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
		if (this.observeAccessReads)
			this.read.add(template);
		return template;
	}

	@Override
	public T getBy(StrolchTransaction tx, String type, String id) {
		T element = this.elementMap.getBy(tx, type, id);
		if (this.observeAccessReads)
			this.read.add(element);
		return element;
	}

	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP, boolean assertExists) throws StrolchException {
		T element = this.elementMap.getBy(tx, refP, assertExists);
		if (this.observeAccessReads)
			this.read.add(element);
		return element;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP, boolean assertExists) throws StrolchException {
		List<T> elements = this.elementMap.getBy(tx, refP, assertExists);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		List<T> elements = this.elementMap.getAllElements(tx);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		List<T> elements = this.elementMap.getElementsBy(tx, type);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
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

	@Override
	public void add(StrolchTransaction tx, T element) {
		this.elementMap.add(tx, element);
		this.created.add(element);
	}

	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) {
		this.elementMap.addAll(tx, elements);
		this.created.addAll(elements);
	}

	@Override
	public T update(StrolchTransaction tx, T element) {
		T replaced = this.elementMap.update(tx, element);
		this.updated.add(element);
		return replaced;

	}

	@Override
	public List<T> updateAll(StrolchTransaction tx, List<T> elements) {
		List<T> replaced = this.elementMap.updateAll(tx, elements);
		this.updated.addAll(elements);
		return replaced;
	}

	@Override
	public void remove(StrolchTransaction tx, T element) {
		this.elementMap.remove(tx, element);
		this.deleted.add(element);
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) {
		this.elementMap.removeAll(tx, elements);
		this.deleted.addAll(elements);
	}

	@Override
	public long removeAll(StrolchTransaction tx) {
		long removed = this.elementMap.removeAll(tx);
		this.deletedAll += removed;
		return removed;
	}

	@Override
	public long removeAllBy(StrolchTransaction tx, String type) {
		long removed = this.elementMap.removeAllBy(tx, type);

		Long byType = this.deletedAllByType.get(type);
		if (byType == null)
			byType = 0L;
		byType = byType + removed;
		this.deletedAllByType.put(type, byType);

		return removed;
	}
}
