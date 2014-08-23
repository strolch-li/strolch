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

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditQuery {

	private int maxResults;

	private List<AuditSelection> selections;

	public AuditQuery() {
		this.selections = new ArrayList<>();
	}

	/**
	 * @return the maxResults
	 */
	public int getMaxResults() {
		return this.maxResults;
	}

	/**
	 * @param maxResults
	 *            the maxResults to set
	 */
	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

	public ActionSelection action() {
		ActionSelection selection = new ActionSelection(this);
		this.selections.add(selection);
		return selection;
	}

	public DateRangeSelection dateRange() {
		DateRangeSelection selection = new DateRangeSelection(this);
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
		for (AuditSelection selection : selections) {
			selection.accept(visitor);
		}
	}
}
