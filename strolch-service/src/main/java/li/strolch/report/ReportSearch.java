package li.strolch.report;

import static li.strolch.report.ReportConstants.TYPE_REPORT;

import java.util.Set;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Privilege;
import li.strolch.search.ResourceSearch;

/**
 * Query to get report resources
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ReportSearch extends ResourceSearch {

	public ReportSearch(StrolchTransaction tx) {
		types(TYPE_REPORT);

		Privilege reportPrivilege = tx.getPrivilegeContext().getPrivilege(ReportSearch.class.getName());
		if (!reportPrivilege.isAllAllowed()) {
			Set<String> allowedReportIds = reportPrivilege.getAllowList();
			where(id().isIn(allowedReportIds));
		}
	}
}
