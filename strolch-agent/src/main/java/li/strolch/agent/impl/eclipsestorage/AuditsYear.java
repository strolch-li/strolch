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

package li.strolch.agent.impl.eclipsestorage;

import li.strolch.model.audit.Audit;
import li.strolch.utils.collections.DateRange;
import org.eclipse.serializer.collections.lazy.LazyHashMap;
import org.eclipse.store.storage.types.StorageManager;

import java.time.Month;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public class AuditsYear {

	private final int year;
	private final LazyHashMap<Month, AuditsMonth> months = new LazyHashMap<>();

	public AuditsYear(int year) {
		this.year = year;
	}

	public int size() {
		return this.months.values().stream().mapToInt(AuditsMonth::size).sum();
	}

	public int size(DateRange dateRange) {
		if (this.months.isEmpty())
			return 0;

		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();
		int fromYear = from.getYear();
		int toYear = to.getYear();
		if (fromYear > this.year || toYear < this.year)
			return 0;

		if (fromYear == toYear && from.getMonth() == to.getMonth()) {
			if (this.months.containsKey(from.getMonth()))
				return this.months.get(from.getMonth()).size(dateRange);
			return 0;
		}

		Month fromMonth = evaluateFromMonth(fromYear, from);
		Month toMonth = evaluateToMonth(toYear, to);

		int size = 0;
		Month currentMonth = fromMonth;
		while (currentMonth.compareTo(toMonth) < 0) {
			if (this.months.containsKey(currentMonth))
				size += this.months.get(currentMonth).size(dateRange);
			currentMonth = currentMonth.plus(1);
		}

		return size;
	}

	public Stream<Audit> audits(DateRange dateRange) {
		if (this.months.isEmpty())
			return Stream.empty();

		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();
		int fromYear = from.getYear();
		int toYear = to.getYear();
		if (fromYear > this.year || toYear < this.year)
			return Stream.empty();

		if (fromYear == toYear && from.getMonth() == to.getMonth()) {
			if (this.months.containsKey(from.getMonth()))
				return this.months.get(from.getMonth()).audits(dateRange);
			return Stream.empty();
		}

		Month fromMonth = evaluateFromMonth(fromYear, from);
		Month toMonth = evaluateToMonth(toYear, to);

		Stream<Audit> stream = Stream.empty();
		Month currentMonth = fromMonth;
		while (currentMonth.compareTo(toMonth) < 0) {
			if (this.months.containsKey(currentMonth))
				stream = Stream.concat(stream, this.months.get(currentMonth).audits(dateRange));
			currentMonth = currentMonth.plus(1);
		}

		return stream;
	}

	private Month evaluateToMonth(int toYear, ZonedDateTime to) {
		Month toMonth;
		if (toYear > this.year)
			toMonth = Month.DECEMBER;
		else
			toMonth = to.getMonth();
		return toMonth;
	}

	private Month evaluateFromMonth(int fromYear, ZonedDateTime from) {
		Month fromMonth;
		if (fromYear < this.year)
			fromMonth = Month.JANUARY;
		else
			fromMonth = from.getMonth();
		return fromMonth;
	}

	public void addAudit(StorageManager storageManager, final Audit audit) {
		ZonedDateTime date = validateAuditDate(audit);
		Month month = date.getMonth();
		AuditsMonth auditsMonth = this.months.computeIfAbsent(month, _ -> new AuditsMonth(this.year, month));
		boolean newMonth = auditsMonth.size() == 0;
		auditsMonth.addAudit(storageManager, audit);
		if (newMonth)
			storageManager.storeAll(this.months, auditsMonth);
	}

	public void addAudits(StorageManager storageManager, List<Audit> audits) {
		audits.forEach(this::validateAuditDate);
		Map<Month, List<Audit>> byMonth = audits.stream().collect(groupingBy(audit -> audit.getDate().getMonth()));
		for (Month month : byMonth.keySet()) {
			AuditsMonth auditsMonth = this.months.computeIfAbsent(month, _ -> new AuditsMonth(this.year, month));
			boolean newMonth = auditsMonth.size() == 0;
			auditsMonth.addAudits(storageManager, audits);
			if (newMonth)
				storageManager.storeAll(this.months, auditsMonth);
		}
	}

	private ZonedDateTime validateAuditDate(Audit audit) {
		ZonedDateTime date = audit.getDate();
		if (date.getYear() != this.year)
			throw new IllegalStateException("Audit year " + date.getYear() + " is not for this year " + this.year);
		return date;
	}
}
