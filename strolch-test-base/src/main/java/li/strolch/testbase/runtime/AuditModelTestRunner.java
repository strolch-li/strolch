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
package li.strolch.testbase.runtime;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.collections.DateRange;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static java.time.ZoneId.systemDefault;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditModelTestRunner {

	private static final Logger log = LoggerFactory.getLogger(AuditModelTestRunner.class);
	private final RuntimeMock runtimeMock;
	private final String realmName;
	private final Certificate certificate;

	public AuditModelTestRunner(RuntimeMock runtimeMock, String realmName) {
		this.runtimeMock = runtimeMock;
		this.realmName = realmName;

		PrivilegeHandler privilegeHandler = runtimeMock.getContainer().getPrivilegeHandler();
		this.certificate = privilegeHandler.authenticate("test", "test".toCharArray());
	}

	public void runTestForAudits() {

		StrolchRealm realm = this.runtimeMock.getRealm(this.realmName);

		Calendar cal = Calendar.getInstance();

		cal.set(2000, Calendar.JULY, 1);
		Date beforeCurrent = cal.getTime();
		cal.set(2000, Calendar.JULY, 2);
		Date current = cal.getTime();
		cal.set(2000, Calendar.JULY, 3);
		Date afterCurrent = cal.getTime();
		cal.set(2000, Calendar.FEBRUARY, 1);
		Date earlier = cal.getTime();
		cal.set(2000, Calendar.DECEMBER, 1);
		Date later = cal.getTime();

		DateRange earlierRange = new DateRange().from(earlier, true).to(beforeCurrent, true);
		DateRange equalsRange = new DateRange().from(current, true).to(current, true);
		DateRange laterRange = new DateRange().from(afterCurrent, true).to(later, true);
		DateRange containsRange = new DateRange().from(earlier, true).to(later, true);
		DateRange noRange = new DateRange().from(LocalDate.EPOCH, true).to(LocalDate.of(2101, 1, 1), true);

		// generate a random audit
		Audit audit = ModelGenerator.randomAudit();
		audit.setDate(current.toInstant().atZone(systemDefault()));

		// single element actions
		{
			// add
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				tx.add(audit);
				tx.commitOnClose();
			}

			// exists
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(1, auditTrail.querySize(tx, equalsRange));
				List<Audit> allElements = auditTrail.getAllElements(tx, audit.getElementType(), equalsRange);
				assertEquals(1, allElements.size());
				assertTrue(allElements.contains(audit));
			}
		}

		// querying
		{
			// querySize
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.getAllElements(tx, noRange).forEach(audit1 -> {
					log.info(audit1.toString());
				});
				assertTrue(auditTrail.querySize(tx) >= 1);
				assertEquals(1, auditTrail.querySize(tx, equalsRange));
				assertEquals(1, auditTrail.getAllElements(tx, equalsRange).size());
				assertEquals(1, auditTrail.querySize(tx, containsRange));
				assertEquals(1, auditTrail.getAllElements(tx, containsRange).size());
				assertEquals(0, auditTrail.querySize(tx, earlierRange));
				assertEquals(0, auditTrail.getAllElements(tx, earlierRange).size());
				assertEquals(0, auditTrail.querySize(tx, laterRange));
				assertEquals(0, auditTrail.getAllElements(tx, laterRange).size());
			}
		}

		// mass element actions
		{
			List<Audit> audits = new ArrayList<>();
			for (int i = 0; i < 100; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setElementType("FooBar");
				randomAudit.setDate(current.toInstant().atZone(systemDefault()));
				audits.add(randomAudit);
			}
			audits.sort(Audit::compareTo);

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				audits.forEach(tx::add);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertTrue(auditTrail.querySize(tx) >= 101);
				assertEquals(101, auditTrail.querySize(tx, containsRange));

				List<Audit> allElements = auditTrail.getAllElements(tx, "FooBar", containsRange);
				allElements.sort(Audit::compareTo);
				assertEquals(audits, allElements);

				allElements = auditTrail.getAllElements(tx, "FooBar", earlierRange);
				assertEquals(0, allElements.size());
				allElements = auditTrail.getAllElements(tx, "FooBar", laterRange);
				assertEquals(0, allElements.size());
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();

				List<Audit> allElements = auditTrail.getAllElements(tx, containsRange);
				allElements.sort(Audit::compareTo);
				assertEquals(101, allElements.size());

				allElements = auditTrail.getAllElements(tx, "FooBar", earlierRange);
				assertEquals(0, allElements.size());
				allElements = auditTrail.getAllElements(tx, "FooBar", laterRange);
				assertEquals(0, allElements.size());
			}
		}
	}
}
