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
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * Command to assign an {@link Action} to a new {@link Resource}.
 *
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class AssignActionCommand extends PlanningCommand {

	private Action action;

	private String targetResourceType;
	private String targetResourceId;

	public AssignActionCommand(StrolchTransaction tx) {
		super(tx);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	public void setTargetResourceType(String type) {
		this.targetResourceType = type;
	}

	public void setTargetResourceId(String id) {
		this.targetResourceId = id;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Action may not be null!", this.action);
		if (this.action.getState() == State.PLANNED) {
			DBC.PRE.assertNotNull("Action attribute resourceId may not be null if action is planned!",
					action.getResourceId());
			DBC.PRE.assertNotNull("Action attribute resourceType may not be null if action is planned!",
					action.getResourceType());
		}
		DBC.PRE.assertNotNull("Target resourceId may not be null!", this.targetResourceId);
		DBC.PRE.assertNotNull("Target resourceType may not be null!", this.targetResourceType);
	}

	@Override
	public void doCommand() {
		Activity rootElement = this.action.getRootElement();
		State currentState = rootElement.getState();
		this.action.accept(this);
		updateOrderState(tx(), rootElement, currentState, rootElement.getState());
	}

	@Override
	public Void visitAction(Action action) {

		// unplan the action
		if (action.getState() == State.PLANNED) {
			PlanningPolicy planningPolicy = tx().getPolicy(action.findPolicy(PlanningPolicy.class, DEFAULT_PLANNING));
			planningPolicy.unplan(action);
		}

		// set target resource
		action.setResourceId(this.targetResourceId);
		action.setResourceType(this.targetResourceType);

		// finally plan the action to the assigned resource
		PlanningPolicy planningPolicy = tx().getPolicy(action.findPolicy(PlanningPolicy.class, DEFAULT_PLANNING));
		planningPolicy.plan(action);

		return null;
	}
}
