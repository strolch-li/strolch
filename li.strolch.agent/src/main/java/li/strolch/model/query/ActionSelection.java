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

import java.util.Arrays;
import java.util.List;

import li.strolch.model.audit.AccessType;
import li.strolch.utils.StringMatchMode;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ActionSelection extends AuditSelection {

	private StringSelection actionSelection;
	private List<AccessType> accessTypes;

	public ActionSelection(AuditQuery<?> query) {
		super(query);
	}

	public ActionSelection actions(StringMatchMode matchMode, String... actions) {
		this.actionSelection = new StringSelection(matchMode, actions);
		return this;
	}

	public ActionSelection accessTypes(List<AccessType> accessTypes) {
		this.accessTypes = accessTypes;
		return this;
	}

	public ActionSelection accessTypes(AccessType... accessTypes) {
		this.accessTypes = Arrays.asList(accessTypes);
		return this;
	}

	public List<AccessType> getAccessTypes() {
		return this.accessTypes;
	}

	public StringSelection getActionSelection() {
		return this.actionSelection;
	}

	public boolean isWildcardActionType() {
		return this.accessTypes == null || this.accessTypes.isEmpty();
	}

	public boolean isWildcardAction() {
		return this.actionSelection == null || this.actionSelection.isWildCard();
	}

	@Override
	public void accept(AuditQueryVisitor visitor) {
		visitor.visit(this);
	}
}
