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

import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class CachedAuditTrail extends TransientAuditTrail {

	private static final Logger logger = LoggerFactory.getLogger(CachedAuditTrail.class);

	private AuditDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getAuditDao(tx);
	}

	@Override
	public synchronized void add(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		super.add(tx, audit);
		// last is to perform DB changes
		getDbDao(tx).save(audit);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		super.addAll(tx, audits);
		// last is to perform DB changes
		getDbDao(tx).saveAll(audits);
	}

	@Override
	public synchronized void update(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		super.update(tx, audit);
		// last is to perform DB changes
		getDbDao(tx).update(audit);
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		super.updateAll(tx, audits);
		// last is to perform DB changes
		getDbDao(tx).updateAll(audits);
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, Audit audit) {
		// first perform cached change
		super.remove(tx, audit);
		// last is to perform DB changes
		getDbDao(tx).remove(audit);
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<Audit> audits) {
		// first perform cached change
		super.removeAll(tx, audits);
		// last is to perform DB changes
		getDbDao(tx).removeAll(audits);
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {

		// first perform cached change
		long removed = super.removeAll(tx, type, dateRange);

		// last is to perform DB changes
		long daoRemoved = getDbDao(tx).removeAll(type, dateRange);
		if (removed != daoRemoved) {
			logger.error("Removed {} elements from cached map, but dao removed {} elements!", removed, daoRemoved);
		}
		return removed;
	}
}
