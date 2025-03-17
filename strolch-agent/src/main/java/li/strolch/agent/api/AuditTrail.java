/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;

import java.util.List;
import java.util.Set;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface AuditTrail {

	/**
	 * Checks if the audit trail is enabled.
	 *
	 * @return true if the audit trail is enabled, false otherwise
	 */
	boolean isEnabled();

	/**
	 * Checks if an audit of the given type and ID exists in the audit trail.
	 *
	 * @param tx   the open transaction
	 * @param type the type of the audit
	 * @param id   the ID of the audit
	 *
	 * @return true if the audit exists, false otherwise
	 */
	boolean hasAudit(StrolchTransaction tx, String type, Long id);

	/**
	 * Queries the total number of audits in the audit trail.
	 *
	 * @param tx the open transaction
	 *
	 * @return the total audit count
	 */
	long querySize(StrolchTransaction tx);

	/**
	 * Queries the number of audits within the given date range.
	 *
	 * @param tx        the open transaction
	 * @param dateRange the date range to filter audits
	 *
	 * @return the count of audits within the date range
	 */
	long querySize(StrolchTransaction tx, DateRange dateRange);

	/**
	 * Queries the number of audits of the specified type within the given date range.
	 *
	 * @param tx        the open transaction
	 * @param type      the type of audits to filter
	 * @param dateRange the date range to filter audits
	 *
	 * @return the count of audits of the specified type within the date range
	 */
	long querySize(StrolchTransaction tx, String type, DateRange dateRange);

	/**
	 * Retrieves all unique audit types available in the audit trail.
	 *
	 * @param tx the open transaction
	 *
	 * @return a set of all available audit types
	 */
	Set<String> getTypes(StrolchTransaction tx);

	/**
	 * Retrieves the audit with the given id, or null if it does not exist
	 *
	 * @param tx the open transaction
	 * @param id the id of the element to retrieve
	 *
	 * @return the element with the type and id, or null if it does not exist
	 */
	Audit getBy(StrolchTransaction tx, String type, Long id);

	/**
	 * Retrieves all audits within the specified date range.
	 *
	 * @param tx        the open transaction
	 * @param dateRange the date range to filter audits
	 *
	 * @return a list of audits within the date range
	 */
	List<Audit> getAllElements(StrolchTransaction tx, DateRange dateRange);

	/**
	 * Retrieves all audits of the specified type within the given date range.
	 *
	 * @param tx        the open transaction
	 * @param type      the type of audits to filter
	 * @param dateRange the date range to filter audits
	 *
	 * @return a list of audits of the specified type within the date range
	 */
	List<Audit> getAllElements(StrolchTransaction tx, String type, DateRange dateRange);

	/**
	 * Adds a single audit to the audit trail.
	 *
	 * @param tx    the open transaction
	 * @param audit the audit to add
	 */
	void add(StrolchTransaction tx, Audit audit);

	/**
	 * Adds multiple audits to the audit trail.
	 *
	 * @param tx     the open transaction
	 * @param audits the audits to add
	 */
	void addAll(StrolchTransaction tx, List<Audit> audits);

	/**
	 * Updates a single audit in the audit trail.
	 *
	 * @param tx    the open transaction
	 * @param audit the audit to update
	 */
	void update(StrolchTransaction tx, Audit audit);

	/**
	 * Updates multiple audits in the audit trail.
	 *
	 * @param tx     the open transaction
	 * @param audits the audits to update
	 */
	void updateAll(StrolchTransaction tx, List<Audit> audits);

	/**
	 * Removes a single audit from the audit trail.
	 *
	 * @param tx    the open transaction
	 * @param audit the audit to remove
	 */
	void remove(StrolchTransaction tx, Audit audit);

	/**
	 * Removes multiple audits from the audit trail.
	 *
	 * @param tx     the open transaction
	 * @param audits the audits to remove
	 */
	void removeAll(StrolchTransaction tx, List<Audit> audits);

	/**
	 * Removes all audits of a specified type within the given date range.
	 *
	 * @param tx        the open transaction
	 * @param type      the type of audits to remove
	 * @param dateRange the date range to filter audits for removal
	 *
	 * @return the number of audits removed
	 */
	long removeAll(StrolchTransaction tx, String type, DateRange dateRange);
}
