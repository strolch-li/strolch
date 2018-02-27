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
package li.strolch.runtime.query.inmemory;

import java.util.ArrayList;
import java.util.List;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * @param <U>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryAuditQuery<U> {

	private AuditTypeNavigator navigator;
	private List<AuditSelector> selectors;
	private AuditVisitor<U> auditVisitor;
	private long limit;

	public InMemoryAuditQuery(AuditTypeNavigator navigator, long limit, List<AuditSelector> selectors,
			AuditVisitor<U> auditVisitor) {
		DBC.PRE.assertNotNull("Navigator must be set!", navigator); //$NON-NLS-1$
		DBC.PRE.assertNotNull("selectors must be set!", selectors); //$NON-NLS-1$
		DBC.PRE.assertNotNull("auditVisitor must be set!", auditVisitor); //$NON-NLS-1$
		this.navigator = navigator;
		this.limit = limit;
		this.selectors = selectors;
		this.auditVisitor = auditVisitor;
	}

	public List<U> doQuery(StrolchTransaction tx, AuditTrail auditTrail) {

		List<U> result = new ArrayList<>();
		List<Audit> elements = this.navigator.navigate(tx, auditTrail);

		elements.sort((a1, a2) -> a2.getDate().compareTo(a1.getDate()));

		for (Audit audit : elements) {

			if (!this.selectors.isEmpty()) {
				boolean nok = false;
				for (AuditSelector selector : this.selectors) {
					if (!selector.select(audit)) {
						nok = true;
						break;
					}
				}

				if (nok)
					continue;
			}

			U returnValue = this.auditVisitor.visitAudit(audit);
			DBC.INTERIM.assertNotNull("Visitor may not return null in query!", returnValue); //$NON-NLS-1$
			result.add(returnValue);

			if (this.limit > 0 && result.size() >= this.limit)
				break;
		}

		return result;
	}
}
