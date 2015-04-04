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

		// element Id
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
