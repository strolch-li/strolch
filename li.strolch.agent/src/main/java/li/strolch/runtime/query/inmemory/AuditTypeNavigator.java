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

import java.util.List;

import li.strolch.agent.api.AuditTrail;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.collections.DateRange;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class AuditTypeNavigator implements AuditNavigator {

	private String type;
	private DateRange dateRange;

	public AuditTypeNavigator(String type, DateRange dateRange) {
		DBC.PRE.assertNotNull("type", type);
		DBC.PRE.assertNotNull("dateRange", dateRange);
		this.type = type;
		this.dateRange = dateRange;
	}

	@Override
	public List<Audit> navigate(StrolchTransaction tx, AuditTrail auditTrail) {
		return auditTrail.getAllElements(tx, this.type, this.dateRange);
	}
}
