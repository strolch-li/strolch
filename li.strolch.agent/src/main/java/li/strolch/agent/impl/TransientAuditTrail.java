package li.strolch.agent.impl;

import java.util.*;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.model.query.AuditQuery;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.query.inmemory.InMemoryAuditQuery;
import li.strolch.runtime.query.inmemory.InMemoryAuditQueryVisitor;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfMaps;

public class TransientAuditTrail implements AuditTrail {

	private MapOfMaps<String, Long, Audit> auditMap;

	public TransientAuditTrail() {
		this.auditMap = new MapOfMaps<>();
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public boolean hasAudit(StrolchTransaction tx, String type, Long id) {
		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return false;
		return byType.containsKey(id);
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
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
	public long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		Map<Long, Audit> byType = this.auditMap.getMap(type);
		if (byType == null)
			return 0L;

		long size = 0L;
		for (Audit audit : byType.values()) {
			if (dateRange.contains(audit.getDate()))
				size++;
		}
		return size;
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return new HashSet<>(this.auditMap.keySet());
	}

	@Override
	public Audit getBy(StrolchTransaction tx, String type, Long id) {
		return this.auditMap.getElement(type, id);
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
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
	public void add(StrolchTransaction tx, Audit audit) {
		this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
	}

	@Override
	public void addAll(StrolchTransaction tx, List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		}
	}

	@Override
	public void update(StrolchTransaction tx, Audit audit) {
		this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
	}

	@Override
	public void updateAll(StrolchTransaction tx, List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.addElement(audit.getElementType(), audit.getId(), audit);
		}
	}

	@Override
	public void remove(StrolchTransaction tx, Audit audit) {
		this.auditMap.removeElement(audit.getElementType(), audit.getId());
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<Audit> audits) {
		for (Audit audit : audits) {
			this.auditMap.removeElement(audit.getElementType(), audit.getId());
		}
	}

	@Override
	public long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {
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
	public <U> List<U> doQuery(StrolchTransaction tx, AuditQuery<U> auditQuery) {
		InMemoryAuditQueryVisitor<U> visitor = new InMemoryAuditQueryVisitor<>();
		InMemoryAuditQuery<U> query = visitor.toInMemory(auditQuery);
		return query.doQuery(tx, this);
	}
}
