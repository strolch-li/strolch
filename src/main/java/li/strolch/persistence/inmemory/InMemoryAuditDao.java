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
package li.strolch.persistence.inmemory;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.persistence.api.AuditDao;
import li.strolch.runtime.query.inmemory.InMemoryAuditQuery;
import li.strolch.runtime.query.inmemory.InMemoryAuditQueryVisitor;
import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.collections.MapOfMaps;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryAuditDao implements AuditDao {

	private MapOfMaps<String, Long, Audit> auditMap;

	public InMemoryAuditDao() {
		this.auditMap = new MapOfMaps<>();
	}

	@Override
	public synchronized boolean hasElement(String type, Long id) {
		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return false;
		return byType.containsKey(id);
	}

	@Override
	public synchronized long querySize(DateRange dateRange) {
		long size = 0L;

		for (String type : this.auditMap.keySet()) {
			Map<Long, Audit> byType = this.auditMap.getMap(type);
			for (Audit audit : byType.values()) {
				if (dateRange.contains(audit.getDate()))
					size++;
			}
		}

		return size;
	}

	@Override
	public synchronized long querySize(String type, DateRange dateRange) {
		long size = 0L;
		Map<Long, Audit> byType = this.auditMap.getMap(type);
		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate()))
				size++;
		}
		return size;
	}

	@Override
	public synchronized Audit queryBy(String type, Long id) {
		return this.auditMap.getElement(type, id);
	}

	@Override
	public synchronized Set<String> queryTypes() {
		return this.auditMap.keySet();
	}

	@Override
	public synchronized List<Audit> queryAll(String type, DateRange dateRange) {
		List<Audit> audits = new ArrayList<>();
		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return audits;

		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate()))
				audits.add(audit);
		}
		return audits;
	}

	@Override
	public synchronized void save(Audit audit) {
		this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
	}

	@Override
	public synchronized void saveAll(List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		}
	}

	@Override
	public synchronized void update(Audit audit) {
		this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
	}

	@Override
	public synchronized void updateAll(List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		}
	}

	@Override
	public synchronized void remove(Audit audit) {
		this.auditMap.removeElement(audit.getElementType(), audit.getId());
	}

	@Override
	public synchronized void removeAll(List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.removeElement(audit.getElementType(), audit.getId());
		}
	}

	@Override
	public synchronized long removeAll(String type, DateRange dateRange) {

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

		return toRemoveList.size();
	}

	@Override
	public <U> List<U> doQuery(AuditQuery auditQuery, AuditVisitor<U> auditVisitor) {
		InMemoryAuditQueryVisitor visitor = new InMemoryAuditQueryVisitor();
		InMemoryAuditQuery<U> query = visitor.visit(auditQuery, auditVisitor);
		return query.doQuery(this);
	}
}
