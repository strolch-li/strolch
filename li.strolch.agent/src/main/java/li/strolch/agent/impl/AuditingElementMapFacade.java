/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the XXX.
 *
 *  XXX is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  XXX is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with XXX.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package li.strolch.agent.impl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.ElementMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.StrolchRootElement;
import li.strolch.model.parameter.StringListParameter;
import li.strolch.model.parameter.StringParameter;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditingElementMapFacade<T extends StrolchRootElement> implements ElementMap<T> {

	private ElementMap<T> elementMap;

	private Set<T> read;
	private Set<T> created;
	private Set<T> updated;
	private Set<T> deleted;
	private long deletedAll;
	private Map<String, Long> deletedAllByType;

	private boolean observeAccessReads;

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

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#hasType(li.strolch.persistence.api.StrolchTransaction, java.lang.String)
	 */
	@Override
	public boolean hasType(StrolchTransaction tx, String type) {
		return this.elementMap.hasType(tx, type);
	}

	/**
	 * @param tx
	 * @param type
	 * @param id
	 * @return
	 * @see li.strolch.agent.api.ElementMap#hasElement(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      java.lang.String)
	 */
	@Override
	public boolean hasElement(StrolchTransaction tx, String type, String id) {
		return this.elementMap.hasElement(tx, type, id);
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.ElementMap#querySize(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public long querySize(StrolchTransaction tx) {
		return this.elementMap.querySize(tx);
	}

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#querySize(li.strolch.persistence.api.StrolchTransaction, java.lang.String)
	 */
	@Override
	public long querySize(StrolchTransaction tx, String type) {
		return this.elementMap.querySize(tx, type);
	}

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getTemplate(li.strolch.persistence.api.StrolchTransaction, java.lang.String)
	 */
	@Override
	public T getTemplate(StrolchTransaction tx, String type) {
		T template = this.elementMap.getTemplate(tx, type);
		if (this.observeAccessReads)
			this.read.add(template);
		return template;
	}

	/**
	 * @param tx
	 * @param type
	 * @param id
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getBy(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      java.lang.String)
	 */
	@Override
	public T getBy(StrolchTransaction tx, String type, String id) {
		T element = this.elementMap.getBy(tx, type, id);
		if (this.observeAccessReads)
			this.read.add(element);
		return element;
	}

	/**
	 * @param tx
	 * @param refP
	 * @return
	 * @throws StrolchException
	 * @see li.strolch.agent.api.ElementMap#getBy(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.parameter.StringParameter)
	 */
	@Override
	public T getBy(StrolchTransaction tx, StringParameter refP) throws StrolchException {
		T element = this.elementMap.getBy(tx, refP);
		if (this.observeAccessReads)
			this.read.add(element);
		return element;
	}

	@Override
	public List<T> getBy(StrolchTransaction tx, StringListParameter refP) throws StrolchException {
		List<T> elements = this.elementMap.getBy(tx, refP);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getAllElements(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public List<T> getAllElements(StrolchTransaction tx) {
		List<T> elements = this.elementMap.getAllElements(tx);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getElementsBy(li.strolch.persistence.api.StrolchTransaction,
	 *      java.lang.String)
	 */
	@Override
	public List<T> getElementsBy(StrolchTransaction tx, String type) {
		List<T> elements = this.elementMap.getElementsBy(tx, type);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getTypes(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return this.elementMap.getTypes(tx);
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getAllKeys(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public Set<String> getAllKeys(StrolchTransaction tx) {
		return this.elementMap.getAllKeys(tx);
	}

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#getKeysBy(li.strolch.persistence.api.StrolchTransaction, java.lang.String)
	 */
	@Override
	public Set<String> getKeysBy(StrolchTransaction tx, String type) {
		return this.elementMap.getKeysBy(tx, type);
	}

	/**
	 * @param tx
	 * @param element
	 * @see li.strolch.agent.api.ElementMap#add(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.StrolchRootElement)
	 */
	@Override
	public void add(StrolchTransaction tx, T element) {
		this.elementMap.add(tx, element);
		this.created.add(element);
	}

	/**
	 * @param tx
	 * @param elements
	 * @see li.strolch.agent.api.ElementMap#addAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public void addAll(StrolchTransaction tx, List<T> elements) {
		this.elementMap.addAll(tx, elements);
		this.created.addAll(elements);
	}

	/**
	 * @param tx
	 * @param element
	 * @return
	 * @see li.strolch.agent.api.ElementMap#update(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.StrolchRootElement)
	 */
	@Override
	public T update(StrolchTransaction tx, T element) {
		T replaced = this.elementMap.update(tx, element);
		this.updated.add(element);
		return replaced;

	}

	/**
	 * @param tx
	 * @param elements
	 * @return
	 * @see li.strolch.agent.api.ElementMap#updateAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public List<T> updateAll(StrolchTransaction tx, List<T> elements) {
		List<T> replaced = this.elementMap.updateAll(tx, elements);
		this.updated.addAll(elements);
		return replaced;
	}

	/**
	 * @param tx
	 * @param element
	 * @see li.strolch.agent.api.ElementMap#remove(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.StrolchRootElement)
	 */
	@Override
	public void remove(StrolchTransaction tx, T element) {
		this.elementMap.remove(tx, element);
		this.deleted.add(element);
	}

	/**
	 * @param tx
	 * @param elements
	 * @see li.strolch.agent.api.ElementMap#removeAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public void removeAll(StrolchTransaction tx, List<T> elements) {
		this.elementMap.removeAll(tx, elements);
		this.deleted.addAll(elements);
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.ElementMap#removeAll(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public long removeAll(StrolchTransaction tx) {
		long removed = this.elementMap.removeAll(tx);
		this.deletedAll += removed;
		return removed;
	}

	/**
	 * @param tx
	 * @param type
	 * @return
	 * @see li.strolch.agent.api.ElementMap#removeAllBy(li.strolch.persistence.api.StrolchTransaction, java.lang.String)
	 */
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
