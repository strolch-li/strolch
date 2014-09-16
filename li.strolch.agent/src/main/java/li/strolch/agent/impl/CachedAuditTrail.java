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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchTransaction;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.collections.MapOfMaps;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CachedAuditTrail implements AuditTrail {

	private static final Logger logger = LoggerFactory.getLogger(CachedAuditTrail.class);

	private MapOfMaps<String, Long, Audit> auditMap;

	public CachedAuditTrail() {
		this.auditMap = new MapOfMaps<>();
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	protected AuditDao getDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getAuditDao(tx);
	}

	@Override
	public synchronized boolean hasAudit(StrolchTransaction tx, String type, Long id) {

		return this.auditMap.containsElement(type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		long size = 0;

		for (Audit audit : this.auditMap.getAllElements()) {
			if (dateRange.contains(audit.getDate()))
				size++;
		}

		return size;
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		long size = 0;

		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return size;

		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate()))
				size++;
		}

		return size;
	}

	@Override
	public synchronized Set<String> getTypes(StrolchTransaction tx) {
		return new HashSet<>(this.auditMap.keySet());
	}

	@Override
	public synchronized Audit getBy(StrolchTransaction tx, String type, Long id) {
		return this.auditMap.getElement(type, id);
	}

	@Override
	public synchronized List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		List<Audit> elements = new ArrayList<>();

		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return elements;

		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate()))
				elements.add(audit);
		}

		return elements;
	}

	@Override
	public synchronized void add(StrolchTransaction tx, Audit audit) {
		this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		// last is to perform DB changes
		getDao(tx).save(audit);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		}
		// last is to perform DB changes
		getDao(tx).saveAll(audits);
	}

	@Override
	public synchronized Audit update(StrolchTransaction tx, Audit audit) {
		Audit replacedAudit = this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		// last is to perform DB changes
		getDao(tx).update(audit);
		return replacedAudit;
	}

	@Override
	public synchronized List<Audit> updateAll(StrolchTransaction tx, List<Audit> audits) {
		List<Audit> replacedAudits = new ArrayList<>();
		for (Audit audit : audits) {
			Audit replacedAudit = this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
			if (replacedAudit != null)
				replacedAudits.add(replacedAudit);
		}
		// last is to perform DB changes
		getDao(tx).updateAll(audits);
		return replacedAudits;
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, Audit audit) {
		this.auditMap.removeElement(audit.getElementType(), audit.getId());
		// last is to perform DB changes
		getDao(tx).remove(audit);
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.removeElement(audit.getElementType(), audit.getId());
		}

		// last is to perform DB changes
		getDao(tx).removeAll(audits);
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {

		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return 0L;

		List<Audit> toRemoveList = new ArrayList<>();

		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate())) {
				toRemoveList.add(audit);
			}
		}

		for (Audit toRemove : toRemoveList) {
			this.auditMap.removeElement(type, toRemove.getId());
		}
		long removed = toRemoveList.size();

		// last is to perform DB changes
		long daoRemoved = getDao(tx).removeAll(type, dateRange);
		if (removed != daoRemoved) {
			String msg = "Removed {0} elements from cached map, but dao removed {1} elements!"; //$NON-NLS-1$
			logger.error(MessageFormat.format(msg, removed, daoRemoved));
		}
		return removed;
	}
}
