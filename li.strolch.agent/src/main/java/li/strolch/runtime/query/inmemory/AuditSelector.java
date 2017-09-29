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

import java.util.HashSet;
import java.util.Set;

import li.strolch.model.audit.*;
import li.strolch.model.query.StringSelection;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class AuditSelector {

	public abstract boolean select(Audit audit);

	public static AuditSelector selectorFor(ElementSelection selection) {
		return new ElementSelector(selection);
	}

	public static AuditSelector selectorFor(IdentitySelection selection) {
		return new IdentitySelector(selection);
	}

	public static AuditSelector selectorFor(ActionSelection selection) {
		return new ActionSelector(selection);
	}

	private static class ElementSelector extends AuditSelector {
		private StringSelection elementSubTypeSelection;
		private StringSelection elementAccessedSelection;

		public ElementSelector(ElementSelection selection) {
			this.elementSubTypeSelection = selection.getElementSubTypeSelection();
			this.elementAccessedSelection = selection.getElementAccessedSelection();
		}

		@Override
		public boolean select(Audit audit) {

			if (this.elementSubTypeSelection != null) {
				if (!this.elementSubTypeSelection.matches(audit.getElementSubType()))
					return false;
			}

			if (this.elementAccessedSelection != null) {
				if (!this.elementAccessedSelection.matches(audit.getElementAccessed()))
					return false;
			}

			return true;
		}
	}

	public static class IdentitySelector extends AuditSelector {

		private StringSelection firstnameSelection;
		private StringSelection lastnameSelection;
		private StringSelection usernameSelection;

		public IdentitySelector(IdentitySelection selection) {
			this.firstnameSelection = selection.getFirstnameSelection();
			this.lastnameSelection = selection.getLastnameSelection();
			this.usernameSelection = selection.getUsernameSelection();
		}

		@Override
		public boolean select(Audit audit) {

			if (this.firstnameSelection != null) {
				if (!this.firstnameSelection.matches(audit.getFirstname()))
					return false;
			}

			if (this.lastnameSelection != null) {
				if (!this.lastnameSelection.matches(audit.getLastname()))
					return false;
			}

			if (this.usernameSelection != null) {
				if (!this.usernameSelection.matches(audit.getUsername()))
					return false;
			}

			return true;
		}
	}

	public static class ActionSelector extends AuditSelector {

		private Set<AccessType> accessTypes;
		private StringSelection actionSelection;

		public ActionSelector(ActionSelection selection) {
			if (selection.getAccessTypes() != null && !selection.getAccessTypes().isEmpty())
				this.accessTypes = new HashSet<>(selection.getAccessTypes());
			this.actionSelection = selection.getActionSelection();
		}

		@Override
		public boolean select(Audit audit) {

			if (this.accessTypes != null) {
				if (!this.accessTypes.contains(audit.getAccessType()))
					return false;
			}

			if (this.actionSelection != null) {
				if (!this.actionSelection.matches(audit.getAction()))
					return false;
			}

			return true;
		}
	}
}
