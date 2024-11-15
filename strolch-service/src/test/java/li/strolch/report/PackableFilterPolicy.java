/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.model.Resource;
import li.strolch.model.parameter.BooleanParameter;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.report.policy.ReportFilterPolicy;
import li.strolch.utils.dbc.DBC;

public class PackableFilterPolicy extends ReportFilterPolicy {

	public PackableFilterPolicy(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public boolean filter(Object value) {
		throw new UnsupportedOperationException("2 values required!");
	}

	@Override
	public boolean filter(Object value1, Object value2) {
		DBC.PRE.assertNotNull("value1 required!", value1);
		DBC.PRE.assertNotNull("value2 required!", value2);

		BooleanParameter productPackable = (BooleanParameter) value1;
		Resource location = (Resource) value2;
		BooleanParameter locationPackable = location.getParameter("packingLocation", false);
		if (locationPackable == null)
			return this.negate;
		boolean packable = productPackable.getValue() == locationPackable.getValue();
		return this.negate != packable;
	}

	@Override
	protected boolean filter(Object left, Object right, boolean negate) {
		throw new UnsupportedOperationException("Not used");
	}
}
