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

import li.strolch.model.audit.ActionSelection;
import li.strolch.model.audit.AuditQuery;
import li.strolch.model.audit.AuditQueryVisitor;
import li.strolch.model.audit.AuditVisitor;
import li.strolch.model.audit.ElementSelection;
import li.strolch.model.audit.IdentitySelection;
import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryAuditQueryVisitor<U> implements AuditQueryVisitor {

	private AuditTypeNavigator navigator;
	private List<AuditSelector> selectors;

	public InMemoryAuditQuery<U> toInMemory(AuditQuery<U> auditQuery) {
		AuditVisitor<U> auditVisitor = auditQuery.getAuditVisitor();
		DBC.PRE.assertNotNull("auditVisitor", auditVisitor); //$NON-NLS-1$

		this.selectors = new ArrayList<>();
		auditQuery.accept(this);

		if (this.navigator == null) {
			String msg = "Query is missing a navigation!"; //$NON-NLS-1$
			throw new QueryException(msg);
		}

		long limit = auditQuery.getLimit();

		return new InMemoryAuditQuery<>(this.navigator, limit, this.selectors, auditVisitor);
	}

	@Override
	public void visit(ElementSelection selection) {
		this.selectors.add(AuditSelector.selectorFor(selection));
	}

	@Override
	public void visit(IdentitySelection selection) {
		this.selectors.add(AuditSelector.selectorFor(selection));
	}

	@Override
	public void visit(ActionSelection selection) {
		this.selectors.add(AuditSelector.selectorFor(selection));
	}

	@Override
	public void visit(AuditQuery<?> auditQuery) {
		String type = auditQuery.getElementTypeSelection();
		DateRange dateRange = auditQuery.getDateRange();
		this.navigator = new AuditTypeNavigator(type, dateRange);
	}
}
