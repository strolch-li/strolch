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
package li.strolch.model.query;

import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class StrolchElementQuery<T extends QueryVisitor> implements StrolchQuery {

	protected Navigation navigation;
	protected Selection selection;

	public StrolchElementQuery() {
		// empty
	}

	public StrolchElementQuery(Navigation navigation) {
		this.navigation = navigation;
	}

	public void setNavigation(Navigation navigation) {
		this.navigation = navigation;
	}

	public boolean hasNavigation() {
		return this.navigation != null;
	}

	public Navigation getNavigation() {
		return this.navigation;
	}

	public boolean hasSelection() {
		return this.selection != null && this.selection.hasSelection();
	}

	public Selection getSelection() {
		return this.selection;
	}

	public StrolchElementQuery<T> with(Selection selection) {
		assertNoSelectionYetSet();
		this.selection = selection;
		return this;
	}

	private void assertNoSelectionYetSet() {
		String msg = "A selection is already set! Use a cascaded boolean operators to perform multiple selections"; //$NON-NLS-1$
		DBC.PRE.assertNull(msg, this.selection);
	}

	public StrolchElementQuery<T> withAny() {
		assertNoSelectionYetSet();
		this.selection = new AnySelection();
		return this;
	}

	public AndSelection and() {
		assertNoSelectionYetSet();
		AndSelection and = new AndSelection();
		this.selection = and;
		return and;
	}

	public OrSelection or() {
		assertNoSelectionYetSet();
		OrSelection or = new OrSelection();
		this.selection = or;
		return or;
	}

	public StrolchElementQuery<T> not(Selection selection) {
		assertNoSelectionYetSet();
		this.selection = new NotSelection(selection);
		return this;
	}

	public void accept(T visitor) {
		DBC.PRE.assertNotNull("No navigation set!", this.navigation); //$NON-NLS-1$
		DBC.PRE.assertNotNull("No selection defined!", this.selection); //$NON-NLS-1$
		this.navigation.accept(visitor);
		this.selection.accept(visitor);
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
