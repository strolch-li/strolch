/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.rest.model.visitor;

import java.util.List;

import li.strolch.model.audit.AccessType;
import li.strolch.rest.model.ActionSelection;
import li.strolch.rest.model.AuditQuery;
import li.strolch.rest.model.DateRange;
import li.strolch.rest.model.IdentitySelection;
import ch.eitchnet.utils.StringMatchMode;
import ch.eitchnet.utils.helper.StringHelper;

public class ToAuditQueryVisitor {

	public li.strolch.model.audit.AuditQuery create(AuditQuery query) {

		// validate element type
		String elementType = query.getElementType();
		if (StringHelper.isEmpty(elementType)) {
			throw new IllegalArgumentException("elementType on AuditQuery is empty!");
		}

		// validate date range
		DateRange dateRange = query.getDateRange();
		if (dateRange == null || dateRange.getFromDate() == null || dateRange.getToDate() == null) {
			throw new IllegalArgumentException("DateRange on AuditQuery is not valid or is missing!");
		}
		ch.eitchnet.utils.collections.DateRange dr = new ch.eitchnet.utils.collections.DateRange().from(
				dateRange.getFromDate(), dateRange.isFromInclusive()).to(dateRange.getToDate(),
				dateRange.isToInclusive());

		// create query
		li.strolch.model.audit.AuditQuery auditQuery = new li.strolch.model.audit.AuditQuery(elementType, dr);

		// limit
		auditQuery.limit(query.getLimit());

		// element
		String elementSubType = query.getElementSubType();
		if (StringHelper.isNotEmpty(elementSubType)) {
			auditQuery.element().elementSubTypes(StringMatchMode.ci(), elementSubType);
		}
		String elementId = query.getElementId();
		if (StringHelper.isNotEmpty(elementId)) {
			auditQuery.element().elementAccessed(StringMatchMode.ci(), elementId);
		}

		// action
		ActionSelection action = query.getAction();
		if (action != null) {
			String actionS = action.getAction();
			li.strolch.model.audit.ActionSelection actionSelection = auditQuery.action();
			if (StringHelper.isNotEmpty(actionS))
				actionSelection.actions(StringMatchMode.ci(), actionS);
			List<AccessType> accessTypes = action.getAccessTypes();
			if (accessTypes != null && !accessTypes.isEmpty()) {
				AccessType[] accessTypesArr = new AccessType[accessTypes.size()];
				accessTypes.toArray(accessTypesArr);
				actionSelection.accessTypes(accessTypesArr);
			}
		}

		// identity
		IdentitySelection identity = query.getIdentity();
		if (identity != null) {
			li.strolch.model.audit.IdentitySelection identitySelection = auditQuery.identity();
			if (StringHelper.isNotEmpty(identity.getFirstname()))
				identitySelection.firstnames(StringMatchMode.ci(), identity.getFirstname());
			if (StringHelper.isNotEmpty(identity.getLastname()))
				identitySelection.lastnames(StringMatchMode.ci(), identity.getLastname());
			if (StringHelper.isNotEmpty(identity.getUsername()))
				identitySelection.usernames(StringMatchMode.ci(), identity.getUsername());
		}

		return auditQuery;
	}
}
