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

import java.util.List;

import li.strolch.execution.policy.PlanningPolicy;
import li.strolch.model.State;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.utils.dbc.DBC;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class ShiftActionCommand extends PlanningCommand {

	private Action action;
	private Long shift;

	public ShiftActionCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Action may not be null!", this.action);
		DBC.PRE.assertNotNull("Action attribute resourceId may not be null!", action.getResourceId());
		DBC.PRE.assertNotNull("Action attribute resourceType may not be null!", action.getResourceType());
		DBC.PRE.assertNotNull("The time to shift the action may not be null!", shift);
	}

	public void setAction(Action action) {
		this.action = action;
	}

	public void setShift(Long shift) {
		this.shift = shift;
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
			if (action.getState() == State.CREATED)
				getConfirmationPolicy(action).toCreated(action);
		}

		// iterate all changes and shift
		final List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (final IValueChange<?> change : changes) {
			change.setTime(change.getTime() + shift);
		}

		// finally plan the action
		PlanningPolicy planningPolicy = tx().getPolicy(action.findPolicy(PlanningPolicy.class, DEFAULT_PLANNING));
		planningPolicy.plan(action);
		if (action.getState() == State.PLANNED)
			getConfirmationPolicy(action).toPlanned(action);
		return null;
	}
}
