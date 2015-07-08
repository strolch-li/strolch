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
package li.strolch.command.plan;

import java.util.List;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.State;
import li.strolch.model.timevalue.IValue;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import ch.eitchnet.utils.dbc.DBC;

/**
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class ShiftActionCommand extends PlanActionCommand {

	private Long shift;

	/**
	 * @param container
	 * @param tx
	 */
	public ShiftActionCommand(final ComponentContainer container, final StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Action may not be null!", this.action);
		DBC.PRE.assertNotNull("Action attribute resourceId may not be null!", action.getResourceId());
		DBC.PRE.assertNotNull("Action attribute resourceType may not be null!", action.getResourceType());
		DBC.PRE.assertNotNull("The time to shift the action may not be null!", shift);
	}

	@Override
	public void doCommand() {

		validate();

		// unplan the action
		if (action.getState() == State.PLANNED)
			unplan(action);

		// iterate all changes and shift
		final List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (final IValueChange<?> change : changes) {
			change.setTime(change.getTime() + shift);
		}

		// finally plan the action
		plan(action);

	}

	@Override
	public void undo() {

		// unplan the action
		if (action.getState() == State.PLANNED)
			unplan(action);

		// iterate all changes and shift
		final List<IValueChange<? extends IValue<?>>> changes = action.getChanges();
		for (final IValueChange<?> change : changes) {
			change.setTime(change.getTime() - shift);
		}

		// finally plan the action
		plan(action);
	}

	public void setShift(Long shift) {
		this.shift = shift;
	}
}
