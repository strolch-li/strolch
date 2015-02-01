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
import java.util.List;
import java.util.Set;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.inmemory.InMemoryAuditDao;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CachedAuditTrail implements AuditTrail {

	private static final Logger logger = LoggerFactory.getLogger(CachedAuditTrail.class);

	private AuditDao cachedDao;

	public CachedAuditTrail() {
		this.cachedDao = new InMemoryAuditDao();
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	protected AuditDao getCachedDao() {
		return this.cachedDao;
	}

	protected AuditDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getAuditDao(tx);
	}

	@Override
	public synchronized boolean hasAudit(StrolchTransaction tx, String type, Long id) {
		return getCachedDao().hasElement(type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		return getCachedDao().querySize(dateRange);
	}

	@Override
	public synchronized long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		return getCachedDao().querySize(type, dateRange);
	}

	@Override
	public synchronized Set<String> getTypes(StrolchTransaction tx) {
		return getCachedDao().queryTypes();
	}

	@Override
	public synchronized Audit getBy(StrolchTransaction tx, String type, Long id) {
		return getCachedDao().queryBy(type, id);
	}

	@Override
	public synchronized List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		return getCachedDao().queryAll(type, dateRange);
	}

	@Override
	public synchronized void add(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		getCachedDao().save(audit);
		// last is to perform DB changes
		getDbDao(tx).save(audit);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		getCachedDao().saveAll(audits);
		// last is to perform DB changes
		getDbDao(tx).saveAll(audits);
	}

	// TODO for update we should return the updated elements, or remove the return value

	@Override
	public synchronized Audit update(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		getCachedDao().update(audit);
		// last is to perform DB changes
		getDbDao(tx).update(audit);
		return audit;
	}

	@Override
	public synchronized List<Audit> updateAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		getCachedDao().updateAll(audits);
		// last is to perform DB changes
		getDbDao(tx).updateAll(audits);
		return audits;
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		getCachedDao().remove(audit);
		// last is to perform DB changes
		getDbDao(tx).remove(audit);
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		getCachedDao().removeAll(audits);
		// last is to perform DB changes
		getDbDao(tx).removeAll(audits);
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {
		// first perform cached change
		long removed = getCachedDao().removeAll(type, dateRange);

		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAll(type, dateRange);
		if (removed != daoRemoved) {
			String msg = "Removed {0} elements from cached map, but dao removed {1} elements!"; //$NON-NLS-1$
			logger.error(MessageFormat.format(msg, removed, daoRemoved));
		}
		return removed;
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, AuditQuery query, AuditVisitor<U> auditVisitor) {
		return getDbDao(tx).doQuery(query, auditVisitor);
	}
}
