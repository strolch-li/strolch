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
public class ElementSelection extends AuditSelection {

	private StringSelection elementSubTypeSelection;
	private StringSelection elementAccessedSelection;

	public ElementSelection(AuditQuery query) {
		super(query);
	}

	public ElementSelection elementAccessed(StringMatchMode matchMode, String... elementsAccessed) {
		this.elementAccessedSelection = new StringSelection(matchMode, elementsAccessed);
		return this;
	}

	public StringSelection getElementAccessedSelection() {
		return this.elementAccessedSelection;
	}

	public boolean isElementAccessedWildcard() {
		return this.elementAccessedSelection == null || this.elementAccessedSelection.isWildCard();
	}

	public ElementSelection elementSubTypes(StringMatchMode matchMode, String... elementSubTypes) {
		this.elementSubTypeSelection = new StringSelection(matchMode, elementSubTypes);
		return this;
	}

	public StringSelection getElementSubTypeSelection() {
		return this.elementSubTypeSelection;
	}

	public boolean isElementSubTypesWildcard() {
		return this.elementSubTypeSelection == null || this.elementSubTypeSelection.isWildCard();
	}

	@Override
	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
	}
}
