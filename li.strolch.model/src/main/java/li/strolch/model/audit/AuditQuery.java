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
package li.strolch.model.audit;

import java.util.ArrayList;
import java.util.List;

import li.strolch.model.query.StrolchQuery;
import ch.eitchnet.utils.collections.DateRange;
import ch.eitchnet.utils.dbc.DBC;

/**
 * 
 * <p>
 * The {@link AuditVisitor} is intended for situations where the query result should not be {@link Audit} but some other
 * object type. For instance in a restful API, the result might have to be mapped to a POJO, thus using this method can
 * perform the mapping step for you
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditQuery<U> implements StrolchQuery {

	protected String elementTypeSelection;
	protected List<AuditSelection> selections;
	protected DateRange dateRange;
	protected long limit;
	protected AuditVisitor<U> auditVisitor;

	public AuditQuery(AuditVisitor<U> auditVisitor, String elementTypeSelection, DateRange dateRange) {
		DBC.PRE.assertNotNull("auditVisitor", auditVisitor);
		DBC.PRE.assertNotEmpty("No elementTypeSelection (navigation) set!", elementTypeSelection); //$NON-NLS-1$
		DBC.PRE.assertFalse("dateRange may not be unbounded!", dateRange.isUnbounded());
		this.auditVisitor = auditVisitor;
		this.elementTypeSelection = elementTypeSelection;
		this.dateRange = dateRange;
		this.selections = new ArrayList<>();
	}

	public AuditVisitor<U> getAuditVisitor() {
		return this.auditVisitor;
	}

	public AuditQuery<U> setAuditVisitor(AuditVisitor<U> auditVisitor) {
		DBC.PRE.assertNotNull("auditVisitor", auditVisitor);
		this.auditVisitor = auditVisitor;
		return this;
	}

	public String getElementTypeSelection() {
		return this.elementTypeSelection;
	}

	public DateRange getDateRange() {
		return this.dateRange;
	}

	public long getLimit() {
		return limit;
	}

	public AuditQuery<U> limit(long limit) {
		this.limit = limit;
		return this;
	}

	public ActionSelection action() {
		ActionSelection selection = new ActionSelection(this);
		this.selections.add(selection);
		return selection;
	}

	public ElementSelection element() {
		ElementSelection selection = new ElementSelection(this);
		this.selections.add(selection);
		return selection;
	}

	public IdentitySelection identity() {
		IdentitySelection selection = new IdentitySelection(this);
		this.selections.add(selection);
		return selection;
	}

	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
		for (AuditSelection selection : this.selections) {
			selection.accept(visitor);
		}
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeName()
	 */
	@Override
	public String getPrivilegeName() {
		return StrolchQuery.class.getName();
	}

	/**
	 * @see ch.eitchnet.privilege.model.Restrictable#getPrivilegeValue()
	 */
	@Override
	public Object getPrivilegeValue() {
		return getClass().getName();
	}

	public static AuditQuery<Audit> query(String elementTypeSelection, DateRange dateRange) {
		return new AuditQuery<>(new NoStrategyAuditVisitor(), elementTypeSelection, dateRange);
	}

	public static <U> AuditQuery<U> query(String elementTypeSelection, DateRange dateRange, AuditVisitor<U> orderVisitor) {
		return new AuditQuery<>(orderVisitor, elementTypeSelection, dateRange);
	}
}
