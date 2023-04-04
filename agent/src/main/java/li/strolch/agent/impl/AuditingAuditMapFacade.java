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

import java.util.*;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.dbc.DBC;

/**
 * <p>
 * This {@link AuditTrail} facade registers all actions performed i.e. it registers which {@link Audit Audits} are
 * retrieved, created, updated and deleted.
 * </p>
 *
 * <p>
 * In a single transaction an Audit may be created, updated and then deleted - this implementation does not "squash"
 * such actions, but registers them separately
 * </p>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditingAuditMapFacade implements AuditTrail {

	private final AuditTrail auditTrail;

	private final Set<Audit> read;
	private final Set<Audit> created;
	private final Set<Audit> updated;
	private final Set<Audit> deleted;
	private long deletedAll;
	private final Map<String, Long> deletedAllByType;

	private final boolean observeAccessReads;

	public AuditingAuditMapFacade(AuditTrail auditTrail, boolean observeAccessReads) {
		DBC.PRE.assertNotNull("auditTrail must be set!", auditTrail);
		this.auditTrail = auditTrail;
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
	public Set<Audit> getRead() {
		return this.read;
	}

	/**
	 * @return the created
	 */
	public Set<Audit> getCreated() {
		return this.created;
	}

	/**
	 * @return the updated
	 */
	public Set<Audit> getUpdated() {
		return this.updated;
	}

	/**
	 * @return the deleted
	 */
	public Set<Audit> getDeleted() {
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
	public boolean isEnabled() {
		return this.auditTrail.isEnabled();
	}

	@Override
	public boolean hasAudit(StrolchTransaction tx, String type, Long id) {
		return this.auditTrail.hasAudit(tx, type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		return this.auditTrail.querySize(tx, dateRange);
	}

	@Override
	public long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		return this.auditTrail.querySize(tx, type, dateRange);
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return this.auditTrail.getTypes(tx);
	}

	@Override
	public Audit getBy(StrolchTransaction tx, String type, Long id) {
		Audit audit = this.auditTrail.getBy(tx, type, id);
		if (this.observeAccessReads)
			this.read.add(audit);
		return audit;
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		List<Audit> elements = this.auditTrail.getAllElements(tx, type, dateRange);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	@Override
	public void add(StrolchTransaction tx, Audit audit) {
		this.auditTrail.add(tx, audit);
		this.created.add(audit);
	}

	@Override
	public void addAll(StrolchTransaction tx, List<Audit> audits) {
		this.auditTrail.addAll(tx, audits);
		this.created.addAll(audits);
	}

	@Override
	public void update(StrolchTransaction tx, Audit audit) {
		this.auditTrail.update(tx, audit);
		this.updated.add(audit);
	}

	@Override
	public void updateAll(StrolchTransaction tx, List<Audit> audits) {
		this.auditTrail.updateAll(tx, audits);
		this.updated.addAll(audits);
	}

	@Override
	public void remove(StrolchTransaction tx, Audit audit) {
		this.auditTrail.remove(tx, audit);
		this.deleted.add(audit);
	}

	@Override
	public void removeAll(StrolchTransaction tx, List<Audit> audits) {
		this.auditTrail.removeAll(tx, audits);
		this.deleted.addAll(audits);
	}

	@Override
	public long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {
		long removed = this.auditTrail.removeAll(tx, type, dateRange);

		Long byType = this.deletedAllByType.get(type);
		if (byType == null)
			byType = 0L;
		byType = byType + removed;
		this.deletedAllByType.put(type, byType);

		this.deletedAll += removed;
		return removed;
	}
}
