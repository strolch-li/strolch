/*
 * Copyright (c) 2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.AuditDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;

import java.util.List;
import java.util.Set;

public class TransactionalAuditTrail implements AuditTrail {

	@Override
	public boolean isEnabled() {
		return true;
	}

	private AuditDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getAuditDao(tx);
	}

	@Override
	public boolean hasAudit(StrolchTransaction tx, String type, Long id) {
		return getDbDao(tx).hasElement(type, id);
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return getDbDao(tx).querySize();
	}

	@Override
	public long querySize(StrolchTransaction tx, DateRange dateRange) {
		return getDbDao(tx).querySize(dateRange);
	}

	@Override
	public long querySize(StrolchTransaction tx, String type, DateRange dateRange) {
		return getDbDao(tx).querySize(type, dateRange);
	}

	@Override
	public Set<String> getTypes(StrolchTransaction tx) {
		return getDbDao(tx).queryTypes();
	}

	@Override
	public Audit getBy(StrolchTransaction tx, String type, Long id) {
		return getDbDao(tx).queryBy(type, id);
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, DateRange dateRange) {
		return getDbDao(tx).queryAll(dateRange);
	}

	@Override
	public List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange) {
		return getDbDao(tx).queryAll(type, dateRange);
	}

	@Override
	public synchronized void add(StrolchTransaction tx, Audit audit) {
		getDbDao(tx).save(audit);
	}

	@Override
	public synchronized void addAll(StrolchTransaction tx, List<Audit> audits) {
		getDbDao(tx).saveAll(audits);
	}

	@Override
	public synchronized void update(StrolchTransaction tx, Audit audit) {
		getDbDao(tx).update(audit);
	}

	@Override
	public synchronized void updateAll(StrolchTransaction tx, List<Audit> audits) {
		getDbDao(tx).updateAll(audits);
	}

	@Override
	public synchronized void remove(StrolchTransaction tx, Audit audit) {
		getDbDao(tx).remove(audit);
	}

	@Override
	public synchronized void removeAll(StrolchTransaction tx, List<Audit> audits) {
		getDbDao(tx).removeAll(audits);
	}

	@Override
	public synchronized long removeAll(StrolchTransaction tx, String type, DateRange dateRange) {
		return getDbDao(tx).removeAll(type, dateRange);
	}
}
