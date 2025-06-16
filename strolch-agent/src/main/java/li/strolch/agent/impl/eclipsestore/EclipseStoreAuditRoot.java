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

package li.strolch.agent.impl.eclipsestore;

import li.strolch.model.audit.Audit;
import li.strolch.utils.collections.DateRange;
import org.eclipse.store.storage.types.StorageManager;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public class EclipseStoreAuditRoot {

	private final HashMap<Integer, AuditsYear> years = new HashMap<>();

	public long size() {
		return this.years.values().stream().mapToInt(AuditsYear::size).sum();
	}

	public long size(DateRange dateRange) {
		if (this.years.isEmpty())
			return 0;

		int fromYear = dateRange.getFromDateZdt().getYear();
		int toYear = dateRange.getToDateZdt().getYear();

		if (fromYear == toYear) {
			if (this.years.containsKey(fromYear))
				return this.years.get(fromYear).size(dateRange);
			return 0;
		}

		int size = 0;
		for (int i = fromYear; i < toYear; i++) {
			if (this.years.containsKey(i))
				size += this.years.get(i).size(dateRange);
		}

		return size;
	}

	public Stream<Audit> audits(DateRange dateRange) {
		if (this.years.isEmpty())
			return Stream.empty();

		int fromYear = dateRange.getFromDateZdt().getYear();
		int toYear = dateRange.getToDateZdt().getYear();

		if (fromYear == toYear) {
			if (this.years.containsKey(fromYear))
				return this.years.get(fromYear).audits(dateRange);
			return Stream.empty();
		}

		Stream<Audit> stream = Stream.empty();
		for (int i = fromYear; i < toYear; i++) {
			if (this.years.containsKey(i))
				stream = Stream.concat(stream, this.years.get(i).audits(dateRange));
		}

		return stream;
	}

	public void addAudit(StorageManager storageManager, final Audit audit) {
		int year = audit.getDate().getYear();
		AuditsYear auditsYear = this.years.computeIfAbsent(year, _ -> new AuditsYear(year));
		boolean newYear = auditsYear.size() == 0;
		auditsYear.addAudit(storageManager, audit);
		if (newYear)
			storageManager.storeAll(this.years, auditsYear);
	}

	public void addAudits(StorageManager storageManager, List<Audit> audits) {
		Map<Integer, List<Audit>> byYear = audits.stream().collect(groupingBy(audit -> audit.getDate().getYear()));
		for (Integer year : byYear.keySet()) {
			AuditsYear auditsYear = this.years.computeIfAbsent(year, _ -> new AuditsYear(year));
			boolean newYear = auditsYear.size() == 0;
			auditsYear.addAudits(storageManager, byYear.get(year));
			if (newYear)
				storageManager.storeAll(this.years, auditsYear);
		}
	}
}
