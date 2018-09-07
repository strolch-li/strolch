package li.strolch.report;

import static li.strolch.report.ReportConstants.TYPE_REPORT;

import li.strolch.search.ResourceSearch;

/**
 * Query to get report resources
 *
 * @author mvoigt
 */
public class ReportSearch extends ResourceSearch {

	public ReportSearch() {
		types(TYPE_REPORT);
	}
}
