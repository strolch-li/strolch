/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.report;

import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.Privilege;
import li.strolch.search.ResourceSearch;

import java.util.Set;

import static li.strolch.report.ReportConstants.TYPE_REPORT;

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
