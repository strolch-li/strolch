/*
 * Copyright 2015 Martin Smock <martin.smock@bluewin.ch>
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
package li.strolch.execution.command;

import static li.strolch.execution.policy.NoPlanning.DEFAULT_PLANNING;

import li.strolch.execution.policy.PlanningPolicy;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.policy.PolicyDef;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * Command to plan an {@link Activity} to a {@link Resource}. This {@link Command} assumes that the {@link IValueChange}
 * objects of the action are already constructed and {@link Action#getResourceId()} is set.
 *
 * <br>
 * <p>
 * It iterates the {@link IValueChange} operators and registers the resulting changes on the {@link StrolchTimedState}
 * objects assigned to the {@link Resource}.
 *
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class PlanActivityCommand extends PlanningCommand {

	protected Activity activity;

	public PlanActivityCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setActivity(Activity activity) {
		this.activity = activity;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Activity may not be null!", this.activity);
	}

	@Override
	public void doCommand() {
		Activity rootElement = this.activity.getRootElement();
		State currentState = rootElement.getState();
		this.activity.accept(this);
		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}

	@Override
	public Void visitAction(Action action) {
		State currentState = action.getState();
		if (currentState.compareTo(State.PLANNED) >= 0)
			return null;

		PolicyDef planningPolicyDef = action.findPolicy(PlanningPolicy.class, DEFAULT_PLANNING);
		PlanningPolicy planningPolicy = tx().getPolicy(PlanningPolicy.class, planningPolicyDef);
		planningPolicy.plan(action);
		if (action.getState() != currentState)
			getConfirmationPolicy(action).doConfirmation(action);
		return null;
	}
}
