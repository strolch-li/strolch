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
package li.strolch.persistence.api;

import java.util.List;
import java.util.Set;

import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditVisitor;
import ch.eitchnet.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface AuditDao {

	public boolean hasElement(String type, Long id);

	public long querySize(DateRange dateRange);

	public long querySize(String type, DateRange dateRange);

	public Audit queryBy(String type, Long id);

	public Set<String> queryTypes();

	public List<Audit> queryAll(String type, DateRange dateRange);

	public void save(Audit audit);

	public void saveAll(List<Audit> audits);

	public void update(Audit audit);

	public void updateAll(List<Audit> audits);

	public void remove(Audit audit);

	public void removeAll(List<Audit> audits);

	public long removeAll(String type, DateRange dateRange);

	public <U> List<U> doQuery(AuditQuery query, AuditVisitor<U> auditVisitor);
}
