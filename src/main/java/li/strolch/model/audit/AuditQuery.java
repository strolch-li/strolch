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
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditQuery implements StrolchQuery {

	private String elementTypeSelection;
	private List<AuditSelection> selections;
	private DateRange dateRange;

	public AuditQuery(String elementTypeSelection, DateRange dateRange) {
		DBC.PRE.assertFalse("dateRange may not be unbounded!", dateRange.isUnbounded());
		this.elementTypeSelection = elementTypeSelection;
		this.dateRange = dateRange;
		this.selections = new ArrayList<>();
	}

	public String getElementTypeSelection() {
		return this.elementTypeSelection;
	}

	public DateRange getDateRange() {
		return this.dateRange;
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
		DBC.PRE.assertNotNull("No elementTypeSelection (navigation) set!", this.elementTypeSelection); //$NON-NLS-1$
		DBC.PRE.assertNotNull("No dateRange set!", this.dateRange); //$NON-NLS-1$
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
}
