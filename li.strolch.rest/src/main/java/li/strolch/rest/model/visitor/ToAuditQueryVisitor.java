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

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.model.query.ActionSelection;
import li.strolch.model.query.AuditQuery;
import li.strolch.rest.model.AuditQueryData;
import li.strolch.utils.StringMatchMode;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

public class ToAuditQueryVisitor {

	public li.strolch.model.query.AuditQuery<Audit> create(AuditQueryData query) {

		// validate element type
		String elementType = query.getElementType();
		if (StringHelper.isEmpty(elementType)) {
			throw new IllegalArgumentException("elementType on AuditQueryData is empty!");
		}

		// validate date range
		if (query.getFromDate() == null || query.getToDate() == null) {
			throw new IllegalArgumentException("fromDate or toDate must be set!");
		}
		Date fromDate = ISO8601FormatFactory.getInstance().parseDate(query.getFromDate());
		Date toDate = ISO8601FormatFactory.getInstance().parseDate(query.getToDate());
		DateRange dr = new DateRange().from(fromDate, true).to(toDate, false);

		// create query
		AuditQuery<Audit> auditQuery = AuditQuery.query(elementType, dr);

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
		String actionS = query.getAction();
		ActionSelection actionSelection = auditQuery.action();
		if (StringHelper.isNotEmpty(actionS))
			actionSelection.actions(StringMatchMode.ci(), actionS);
		String accessTypesS = query.getAccessTypes();
		if (StringHelper.isNotEmpty(accessTypesS)) {
			String[] accessTypes = accessTypesS.split("\\|");
			List<AccessType> accessTypeList = Arrays.stream(accessTypes).map(AccessType::valueOf)
					.collect(Collectors.toList());
			actionSelection.accessTypes(accessTypeList);
		}

		// identity
		li.strolch.model.query.IdentitySelection identitySelection = auditQuery.identity();
		if (StringHelper.isNotEmpty(query.getFirstname()))
			identitySelection.firstnames(StringMatchMode.ci(), query.getFirstname());
		if (StringHelper.isNotEmpty(query.getLastname()))
			identitySelection.lastnames(StringMatchMode.ci(), query.getLastname());
		if (StringHelper.isNotEmpty(query.getUsername()))
			identitySelection.usernames(StringMatchMode.ci(), query.getUsername());

		return auditQuery;
	}
}
