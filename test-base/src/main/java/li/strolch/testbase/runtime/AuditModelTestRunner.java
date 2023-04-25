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
package li.strolch.testbase.runtime;

import static org.junit.Assert.*;

import java.util.*;

import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.ModelGenerator;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Certificate;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.collections.DateRange;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditModelTestRunner {

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

		// first cleanup
		cleanup(realm);

		// single element actions
		{
			Audit audit = ModelGenerator.randomAudit();
			audit.setDate(current);

			// add
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.add(tx, audit);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				Set<String> types = auditTrail.getTypes(tx);
				assertEquals(1, types.size());
				assertTrue(types.contains(audit.getElementType()));
			}

			// has
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertTrue(auditTrail.hasAudit(tx, audit.getElementType(), audit.getId()));

				Audit dbAudit = auditTrail.getBy(tx, audit.getElementType(), audit.getId());
				assertNotNull(dbAudit);
				assertEquals(audit, dbAudit);

				dbAudit = auditTrail.getBy(tx, "Foo", audit.getId());
				assertNull(dbAudit);
			}

			// remove
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.remove(tx, audit);
				tx.commitOnClose();
			}
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				Audit dbAudit = auditTrail.getBy(tx, audit.getElementType(), audit.getId());
				assertNull(dbAudit);
			}

			// update
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.add(tx, audit);
				tx.commitOnClose();
			}
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				Audit dbAudit = auditTrail.getBy(tx, audit.getElementType(), audit.getId());
				dbAudit.setAction("Foo");
				auditTrail.update(tx, dbAudit);
				tx.commitOnClose();
			}
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				Audit dbAudit = auditTrail.getBy(tx, audit.getElementType(), audit.getId());
				assertEquals("Foo", dbAudit.getAction());
			}
		}

		// querying
		{
			Audit audit = ModelGenerator.randomAudit();
			audit.setDate(current);

			// querySize
			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(1, auditTrail.querySize(tx, audit.getElementType(), equalsRange));
				assertEquals(1, auditTrail.querySize(tx, audit.getElementType(), containsRange));
				assertEquals(0, auditTrail.querySize(tx, audit.getElementType(), earlierRange));
				assertEquals(0, auditTrail.querySize(tx, audit.getElementType(), laterRange));
			}
		}

		// mass element actions
		{
			List<Audit> audits = new ArrayList<>();
			for (int i = 0; i < 100; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setElementType("FooBar");
				randomAudit.setDate(current);
				audits.add(randomAudit);
			}
			audits.sort(new AuditByIdComparator());

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.addAll(tx, audits);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(100, auditTrail.querySize(tx, "FooBar", containsRange));

				List<Audit> allElements = auditTrail.getAllElements(tx, "FooBar", containsRange);
				allElements.sort(new AuditByIdComparator());
				assertEquals(audits, allElements);

				allElements = auditTrail.getAllElements(tx, "FooBar", earlierRange);
				assertEquals(0, allElements.size());
				allElements = auditTrail.getAllElements(tx, "FooBar", laterRange);
				assertEquals(0, allElements.size());
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.removeAll(tx, audits);
				assertEquals(0, auditTrail.querySize(tx, "FooBar", containsRange));
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", true)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(0, auditTrail.querySize(tx, "FooBar", containsRange));
			}
		}

		// update all
		{
			List<Audit> audits = new ArrayList<>();
			for (int i = 0; i < 100; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setElementType("Bar");
				randomAudit.setDate(current);
				randomAudit.setAction("BarFoo");
				audits.add(randomAudit);
			}
			audits.sort(new AuditByIdComparator());

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.addAll(tx, audits);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				List<Audit> allElements = auditTrail.getAllElements(tx, "Bar", containsRange);
				allElements.sort(new AuditByIdComparator());
				assertEquals(audits, allElements);

				for (Audit dbAudit : allElements) {
					assertEquals("BarFoo", dbAudit.getAction());
				}

				for (Audit dbAudit : allElements) {
					dbAudit.setAction("Foo");
				}

				auditTrail.updateAll(tx, allElements);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				List<Audit> allElements = auditTrail.getAllElements(tx, "Bar", containsRange);
				for (Audit dbAudit : allElements) {
					assertEquals("Foo", dbAudit.getAction());
				}
			}
		}

		// remove all
		{
			// first cleanup
			cleanup(realm);

			List<Audit> audits = new ArrayList<>();
			for (int i = 0; i < 5; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setDate(current);
				randomAudit.setElementType("BarBarBar");
				audits.add(randomAudit);
			}
			for (int i = 0; i < 5; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setDate(current);
				randomAudit.setElementType("FooFooFoo");
				audits.add(randomAudit);
			}
			for (int i = 0; i < 5; i++) {
				Audit randomAudit = ModelGenerator.randomAudit();
				randomAudit.setDate(current);
				randomAudit.setElementType("BarFooBar");
				audits.add(randomAudit);
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				auditTrail.addAll(tx, audits);
				tx.commitOnClose();
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(15, auditTrail.querySize(tx, containsRange));
				assertEquals(5, auditTrail.querySize(tx, "BarBarBar", containsRange));
				assertEquals(5, auditTrail.querySize(tx, "FooFooFoo", containsRange));
				assertEquals(5, auditTrail.querySize(tx, "BarFooBar", containsRange));
			}

			try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
				AuditTrail auditTrail = tx.getAuditTrail();
				assertEquals(5, auditTrail.removeAll(tx, "BarBarBar", containsRange));
				assertEquals(10, auditTrail.querySize(tx, containsRange));

				assertEquals(5, auditTrail.removeAll(tx, "FooFooFoo", containsRange));
				assertEquals(5, auditTrail.querySize(tx, containsRange));

				assertEquals(5, auditTrail.removeAll(tx, "BarFooBar", containsRange));
				assertEquals(0, auditTrail.querySize(tx, containsRange));
				tx.commitOnClose();
			}
		}
	}

	private void cleanup(StrolchRealm realm) {

		DateRange dateRange = new DateRange().from(new Date(0), true).to(new Date((long) Math.pow(2, 50)), true);

		try (StrolchTransaction tx = realm.openTx(this.certificate, "test", false)) {
			AuditTrail auditTrail = tx.getAuditTrail();
			Set<String> types = auditTrail.getTypes(tx);
			for (String type : types) {
				auditTrail.removeAll(tx, type, dateRange);
			}

			assertEquals(0, auditTrail.querySize(tx, dateRange));
			tx.commitOnClose();
		}
	}

	private static final class AuditByIdComparator implements Comparator<Audit> {
		@Override
		public int compare(Audit o1, Audit o2) {
			return o1.getId().compareTo(o2.getId());
		}
	}
}
