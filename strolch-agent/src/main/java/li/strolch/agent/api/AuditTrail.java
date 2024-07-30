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
package li.strolch.agent.api;

import java.util.List;
import java.util.Set;

import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface AuditTrail {

	boolean isEnabled();

	boolean hasAudit(StrolchTransaction tx, String type, Long id);

	long querySize(StrolchTransaction tx, DateRange dateRange);

	long querySize(StrolchTransaction tx, String type, DateRange dateRange);

	Set<String> getTypes(StrolchTransaction tx);

	/**
	 * Retrieves the audit with the given id, or null if it does not exist
	 *
	 * @param tx
	 * 		the open transaction
	 * @param id
	 * 		the id of the element to retrieve
	 *
	 * @return the element with the type and id, or null if it does not exist
	 */
	Audit getBy(StrolchTransaction tx, String type, Long id);

	List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange);

	void add(StrolchTransaction tx, Audit audit);

	void addAll(StrolchTransaction tx, List<Audit> audits);

	void update(StrolchTransaction tx, Audit audit);

	void updateAll(StrolchTransaction tx, List<Audit> audits);

	void remove(StrolchTransaction tx, Audit audit);

	void removeAll(StrolchTransaction tx, List<Audit> audits);

	long removeAll(StrolchTransaction tx, String type, DateRange dateRange);
}
