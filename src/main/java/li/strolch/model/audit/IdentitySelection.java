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

import li.strolch.model.query.StringSelection;
import ch.eitchnet.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class IdentitySelection extends AuditSelection {

	private StringSelection usernameSelection;
	private StringSelection firstnameSelection;
	private StringSelection lastnameSelection;

	public IdentitySelection(AuditQuery query) {
		super(query);
	}

	public IdentitySelection usernames(StringMatchMode matchMode, String... usernames) {
		this.usernameSelection = new StringSelection(matchMode, usernames);
		return this;
	}

	public IdentitySelection firstnames(StringMatchMode matchMode, String... firstnames) {
		this.firstnameSelection = new StringSelection(matchMode, firstnames);
		return this;
	}

	public IdentitySelection lastnames(StringMatchMode matchMode, String... lastnames) {
		this.lastnameSelection = new StringSelection(matchMode, lastnames);
		return this;
	}

	/**
	 * @return the firstnameSelection
	 */
	public StringSelection getFirstnameSelection() {
		return this.firstnameSelection;
	}

	/**
	 * @return the lastnameSelection
	 */
	public StringSelection getLastnameSelection() {
		return this.lastnameSelection;
	}

	/**
	 * @return the usernameSelection
	 */
	public StringSelection getUsernameSelection() {
		return this.usernameSelection;
	}

	public boolean isFirstnameWildcard() {
		return this.firstnameSelection == null || this.firstnameSelection.isWildCard();
	}

	public boolean isLastnameWildcard() {
		return this.lastnameSelection == null || this.lastnameSelection.isWildCard();
	}

	public boolean isUsernameWildcard() {
		return this.usernameSelection == null || this.usernameSelection.isWildCard();
	}

	public boolean isWildcard() {
		return isFirstnameWildcard() && isLastnameWildcard() && isUsernameWildcard();
	}

	@Override
	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
	}
}
