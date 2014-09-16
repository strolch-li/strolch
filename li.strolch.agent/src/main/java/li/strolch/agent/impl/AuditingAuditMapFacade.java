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

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditingAuditMapFacade implements AuditTrail {

	private AuditTrail auditTrail;

	private Set<Audit> read;
	private Set<Audit> created;
	private Set<Audit> updated;
	private Set<Audit> deleted;
	private long deletedAll;
	private Map<String, Long> deletedAllByType;

	private boolean observeAccessReads;

	public AuditingAuditMapFacade(AuditTrail auditTrail, boolean observeAccessReads) {
		DBC.PRE.assertNotNull("auditTrail must be set!", auditTrail); //$NON-NLS-1$
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

	/**
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#isEnabled()
	 */
	@Override
	public boolean isEnabled() {
		return this.auditTrail.isEnabled();
	}

	/**
	 * @param tx
	 * @param type
	 * @param id
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#hasAudit(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      java.lang.Long)
	 */
	@Override
	public boolean hasAudit(StrolchTransaction tx, String type, Long id) {
		return this.auditTrail.hasAudit(tx, type, id);
	}

	/**
	 * @param tx
	 * @param dateRange
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#querySize(li.strolch.persistence.api.StrolchTransaction,
	 *      ch.eitchnet.utils.collections.DateRange)
	 */
	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		return this.auditTrail.querySize(tx, dateRange);
	}

	/**
	 * @param tx
	 * @param type
	 * @param dateRange
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#querySize(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      ch.eitchnet.utils.collections.DateRange)
	 */
	@Override
	public long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		return this.auditTrail.querySize(tx, type, dateRange);
	}

	/**
	 * @param tx
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#getTypes(li.strolch.persistence.api.StrolchTransaction)
	 */
	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return this.auditTrail.getTypes(tx);
	}

	/**
	 * @param tx
	 * @param type
	 * @param id
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#getBy(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      java.lang.Long)
	 */
	@Override
	public Audit getBy(StrolchTransaction tx, String type, Long id) {
		Audit audit = this.auditTrail.getBy(tx, type, id);
		if (this.observeAccessReads)
			this.read.add(audit);
		return audit;
	}

	/**
	 * @param tx
	 * @param type
	 * @param dateRange
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#getAllElements(li.strolch.persistence.api.StrolchTransaction,
	 *      java.lang.String, ch.eitchnet.utils.collections.DateRange)
	 */
	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		List<Audit> elements = this.auditTrail.getAllElements(tx, type, dateRange);
		if (this.observeAccessReads)
			this.read.addAll(elements);
		return elements;
	}

	/**
	 * @param tx
	 * @param audit
	 * @see li.strolch.agent.api.AuditTrail#add(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.audit.Audit)
	 */
	@Override
	public void add(StrolchTransaction tx, Audit audit) {
		this.auditTrail.add(tx, audit);
		this.created.add(audit);
	}

	/**
	 * @param tx
	 * @param audits
	 * @see li.strolch.agent.api.AuditTrail#addAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public void addAll(StrolchTransaction tx, List<Audit> audits) {
		this.auditTrail.addAll(tx, audits);
		this.created.addAll(audits);
	}

	/**
	 * @param tx
	 * @param audit
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#update(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.audit.Audit)
	 */
	@Override
	public Audit update(StrolchTransaction tx, Audit audit) {
		Audit replaced = this.auditTrail.update(tx, audit);
		this.updated.add(audit);
		return replaced;
	}

	/**
	 * @param tx
	 * @param audits
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#updateAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public List<Audit> updateAll(StrolchTransaction tx, List<Audit> audits) {
		List<Audit> replaced = this.auditTrail.updateAll(tx, audits);
		this.updated.addAll(audits);
		return replaced;
	}

	/**
	 * @param tx
	 * @param audit
	 * @see li.strolch.agent.api.AuditTrail#remove(li.strolch.persistence.api.StrolchTransaction,
	 *      li.strolch.model.audit.Audit)
	 */
	@Override
	public void remove(StrolchTransaction tx, Audit audit) {
		this.auditTrail.remove(tx, audit);
		this.deleted.add(audit);
	}

	/**
	 * @param tx
	 * @param audits
	 * @see li.strolch.agent.api.AuditTrail#removeAll(li.strolch.persistence.api.StrolchTransaction, java.util.List)
	 */
	@Override
	public void removeAll(StrolchTransaction tx, List<Audit> audits) {
		this.auditTrail.removeAll(tx, audits);
		this.deleted.addAll(audits);
	}

	/**
	 * @param tx
	 * @param type
	 * @param dateRange
	 * @return
	 * @see li.strolch.agent.api.AuditTrail#removeAll(li.strolch.persistence.api.StrolchTransaction, java.lang.String,
	 *      ch.eitchnet.utils.collections.DateRange)
	 */
	@Override
	public long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {
		long removed = this.auditTrail.removeAll(tx, type, dateRange);

		Long byType = this.deletedAllByType.get(type);
		if (byType == null)
			byType = 0L;
		byType = byType + removed;
		this.deletedAllByType.put(type, byType);

		return removed;
	}
}
