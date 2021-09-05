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
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface AuditDao {

	boolean hasElement(String type, Long id);

	long querySize(DateRange dateRange);

	long querySize(String type, DateRange dateRange);

	Audit queryBy(String type, Long id);

	Set<String> queryTypes();

	List<Audit> queryAll(String type, DateRange dateRange);

	void save(Audit audit);

	void saveAll(List<Audit> audits);

	void update(Audit audit);

	void updateAll(List<Audit> audits);

	void remove(Audit audit);

	void removeAll(List<Audit> audits);

	long removeAll(String type, DateRange dateRange);
}
