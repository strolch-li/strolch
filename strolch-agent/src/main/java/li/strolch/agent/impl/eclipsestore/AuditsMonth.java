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
import org.eclipse.serializer.collections.lazy.LazyHashMap;
import org.eclipse.store.storage.types.StorageManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Month;
import java.time.Year;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.stream.Collectors.groupingBy;

public class AuditsMonth {

	private static final Logger log = LoggerFactory.getLogger(AuditsMonth.class);
	private final int year;
	private final Month month;
	private final LazyHashMap<Integer, AuditsDay> days = new LazyHashMap<>();

	public AuditsMonth(int year, Month month) {
		this.year = year;
		this.month = month;
	}

	public int getYear() {
		return this.year;
	}

	public Month getMonth() {
		return this.month;
	}

	public int size() {
		return this.days.values().stream().mapToInt(AuditsDay::size).sum();
	}

	public int size(DateRange dateRange) {
		if (this.days.isEmpty())
			return 0;

		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();
		if (from.getYear() > this.year) {
			return 0;
		}

		int fromDays = evaluateFromDays(from);
		int toDays = evaluateToDays(to, from);
		if (toDays == fromDays) {
			if (this.days.containsKey(fromDays))
				return this.days.get(fromDays).size(dateRange);
			return 0;
		}

		int size = 0;
		for (int i = fromDays; i < toDays; i++) {
			if (this.days.containsKey(i))
				size += this.days.get(i).size(dateRange);
		}

		return size;
	}

	public Stream<Audit> audits(DateRange dateRange) {
		if (this.days.isEmpty())
			return Stream.empty();

		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();

		if (from.getYear() > this.year) {
			log.warn("Date range from year {} is beyond current year {}", from.getYear(), this.year);
			return Stream.empty();
		}

		int fromDays = evaluateFromDays(from);
		int toDays = evaluateToDays(to, from);
		if (toDays == fromDays) {
			if (this.days.containsKey(fromDays))
				return this.days.get(fromDays).audits(dateRange);
			return Stream.empty();
		}

		Stream<Audit> stream = Stream.empty();
		for (int i = fromDays; i < toDays; i++) {
			if (this.days.containsKey(i))
				stream = Stream.concat(stream, this.days.get(i).audits(dateRange));
		}

		return stream;
	}

	private int evaluateToDays(ZonedDateTime to, ZonedDateTime from) {
		int toDays;
		if (to.getYear() > from.getYear() || to.getMonth().getValue() > from.getMonth().getValue())
			toDays = this.month.length(Year.isLeap(from.getYear()));
		else
			toDays = to.getDayOfMonth();
		return toDays;
	}

	private int evaluateFromDays(ZonedDateTime from) {
		int fromDays;
		if (from.getYear() < this.year || from.getMonth().getValue() < this.month.getValue())
			fromDays = 1;
		else
			fromDays = from.getDayOfMonth();
		return fromDays;
	}

	public void addAudit(StorageManager storageManager, final Audit audit) {
		ZonedDateTime date = validateAuditDate(audit);
		int dayOfMonth = date.getDayOfMonth();
		AuditsDay auditsDay = this.days.computeIfAbsent(dayOfMonth,
				_ -> new AuditsDay(this.year, this.month, dayOfMonth));
		boolean newDay = auditsDay.size() == 0;
		auditsDay.addAudit(storageManager, audit);
		if (newDay)
			storageManager.storeAll(this.days, auditsDay);
	}

	public void addAudits(StorageManager storageManager, List<Audit> audits) {
		audits.forEach(this::validateAuditDate);
		Map<Integer, List<Audit>> byDayOfMonth = audits
				.stream()
				.collect(groupingBy(audit -> audit.getDate().getDayOfMonth()));
		for (Integer dayOfMonth : byDayOfMonth.keySet()) {
			AuditsDay auditsDay = this.days.computeIfAbsent(dayOfMonth,
					_ -> new AuditsDay(this.year, this.month, dayOfMonth));
			boolean newDay = auditsDay.size() == 0;
			auditsDay.addAudits(storageManager, audits);
			if (newDay)
				storageManager.storeAll(this.days, auditsDay);
		}
	}

	private ZonedDateTime validateAuditDate(Audit audit) {
		ZonedDateTime date = audit.getDate();
		if (date.getYear() != this.year)
			throw new IllegalStateException("Audit year " + date.getYear() + " is not for this year " + this.year);
		if (date.getMonth() != this.month)
			throw new IllegalStateException("Audit month " + date.getMonth() + " is not for this month " + this.month);
		return date;
	}
}
