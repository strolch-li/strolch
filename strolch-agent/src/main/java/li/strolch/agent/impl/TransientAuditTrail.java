/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.collections.MapOfMaps;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static li.strolch.utils.collections.SynchronizedCollections.synchronizedMapOfMaps;

public class TransientAuditTrail implements AuditTrail {

	private final MapOfMaps<String, Long, Audit> auditMap;

	public TransientAuditTrail() {
		this.auditMap = synchronizedMapOfMaps(new MapOfMaps<>());
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public long querySize(StrolchTransaction tx) {
		return this.auditMap.keySet().stream().map(this.auditMap::getMap).mapToLong(Map::size).sum();
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
	public List<Audit> getAllElements(StrolchTransaction tx, DateRange dateRange) {
		List<Audit> audits = new ArrayList<>();
		List<Audit> allElements = this.auditMap.getAllElements();
		allElements.forEach(audit -> {
			if (dateRange.contains(audit.getDate()))
				audits.add(audit);
		});
		return audits;
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
}
