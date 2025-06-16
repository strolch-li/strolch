/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.execution.policy;

import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

public class NoPlanning extends PlanningPolicy {

	public NoPlanning(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void plan(Action action) {
		DBC.PRE.assertEquals("Can not plan illegal state", State.CREATED, action.getState());
		logger.info("Planning action {}", action.getLocator());
		action.setState(State.PLANNED);
		tx().update(action.getRootElement());
	}

	@Override
	public void unplan(Action action) {
		DBC.PRE.assertEquals("Can not unplan illegal state", State.PLANNED, action.getState());
		action.setState(State.CREATED);
		tx().update(action.getRootElement());
	}
}
