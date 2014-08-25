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

import ch.eitchnet.utils.StringMatchMode;
import li.strolch.model.query.StringSelection;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ElementSelection extends AuditSelection {

	private StringSelection elementTypeSelection;
	private StringSelection elementAccessedSelection;

	public ElementSelection(AuditQuery query) {
		super(query);
	}

	public ElementSelection elementTypes(StringMatchMode matchMode, String... elementTypes) {
		this.elementTypeSelection = new StringSelection(matchMode, elementTypes);
		return this;
	}

	public ElementSelection elementsAccessed(StringMatchMode matchMode, String... elementsAccessed) {
		this.elementAccessedSelection = new StringSelection(matchMode, elementsAccessed);
		return this;
	}

	public StringSelection getElementAccessedSelection() {
		return this.elementAccessedSelection;
	}

	public StringSelection getElementTypeSelection() {
		return this.elementTypeSelection;
	}

	public boolean isElementTypesWildcard() {
		return this.elementTypeSelection == null || this.elementTypeSelection.isWildCard();
	}

	public boolean isElementsAccessedWildcard() {
		return this.elementAccessedSelection == null || this.elementAccessedSelection.isWildCard();
	}

	@Override
	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
	}
}
