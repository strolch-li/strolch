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
import org.eclipse.serializer.collections.lazy.LazyArrayList;
import org.eclipse.serializer.collections.lazy.LazyList;
import org.eclipse.store.storage.types.StorageManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Month;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.stream.Stream;

public class AuditsDay {

	private static final Logger log = LoggerFactory.getLogger(AuditsDay.class);
	private final int year;
	private final Month month;
	private final int day;
	private LazyList<Audit> audits = new LazyArrayList<>();

	public AuditsDay(int year, Month month, int day) {
		this.year = year;
		this.month = month;
		this.day = day;
	}

	public int getYear() {
		return this.year;
	}

	public Month getMonth() {
		return this.month;
	}

	public int getDay() {
		return this.day;
	}

	public int size(DateRange dateRange) {
		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();

		if (from.getYear() > this.year
				|| from.getMonth().getValue() > this.month.getValue()
				|| from.getDayOfMonth() > this.day) {
			return 0;
		}

		if (!(
				to.getYear() > this.year
						|| to.getMonth().getValue() > this.month.getValue()
						|| to.getDayOfMonth() > this.day)) {
			return 0;
		}

		return size();
	}

	public int size() {
		return this.audits.size();
	}

	public Stream<Audit> audits(DateRange dateRange) {
		ZonedDateTime from = dateRange.getFromDateZdt();
		ZonedDateTime to = dateRange.getToDateZdt();

		if (from.getYear() > this.year) {
			log.warn("Date range from year {} is beyond current year {}", from.getYear(), this.year);
			return Stream.empty();
		} else if (from.getMonth().getValue() > this.month.getValue()) {
			log.warn("Date range from month {} is beyond current month {}", from.getMonth(), this.month);
			return Stream.empty();
		} else if (from.getDayOfMonth() > this.day) {
			log.warn("Date range from dayOfMonth {} is beyond current day {}", from.getDayOfMonth(), this.day);
			return Stream.empty();
		}

		if (!(
				to.getYear() > this.year
						|| to.getMonth().getValue() > this.month.getValue()
						|| to.getDayOfMonth() > this.day)) {
			log.warn("Date range to {} is before current day {}-{}-{}", to.toLocalDate(), this.year, this.month,
					this.day);
			return Stream.empty();
		}

		return this.audits.stream();
	}

	public void addAudit(StorageManager storageManager, final Audit audit) {
		validateAuditDate(audit);
		this.audits.add(audit);
		storageManager.store(this.audits);
	}

	public void addAudits(StorageManager storageManager, List<Audit> audits) {
		audits.forEach(this::validateAuditDate);
		this.audits.addAll(audits);
		storageManager.store(this.audits);
	}

	private void validateAuditDate(Audit audit) {
		ZonedDateTime date = audit.getDate();
		if (date.getYear() != this.year)
			throw new IllegalStateException("Audit year " + date.getYear() + " is not for this year " + this.year);
		if (date.getMonth().equals(this.month))
			throw new IllegalStateException("Audit month " + date.getMonth() + " is not for this month " + this.month);
		if (date.getDayOfMonth() != this.day)
			throw new IllegalStateException(
					"Audit dayOfMonth " + date.getDayOfMonth() + " is not for this day " + this.day);
	}
}
