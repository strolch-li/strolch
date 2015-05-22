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
import li.strolch.model.Locator;
import li.strolch.model.Resource;
import li.strolch.model.State;
import li.strolch.model.Tags;
import li.strolch.model.activity.Action;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import ch.eitchnet.utils.dbc.DBC;

/**
 * Command to plan an {@link Action} to a {@link Resource}. This {@link Command}
 * assumes that the {@link IValueChange} objects of the action are already
 * constructed. It iterates the {@link IValueChange} operators and registers the
 * resulting changes on the {@link StrolchTimedState} objects assigned to the
 * {@link Resource}.
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class PlanActionCommand extends Command {

	protected Action action;

	/**
	 * @param container
	 * @param tx
	 */
	public PlanActionCommand(final ComponentContainer container, final StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Action may not be null!", this.action);
		DBC.PRE.assertNotNull("Action attribute resourceId may not be null!", this.action.getResourceId());
		DBC.PRE.assertNotNull("Action attribute resourceType may not be null!", this.action.getResourceType());
		DBC.PRE.assertTrue("Action attribute start must be set!", this.action.getStart() > 0);
		DBC.PRE.assertTrue("Action attribute end must later than start!", this.action.getEnd() > this.action.getStart());
	}

	@Override
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void doCommand() {

		Locator locator = Locator.newBuilder(Tags.RESOURCE, action.getResourceType(), action.getResourceId()).build();
		Resource resource = tx().findElement(locator);
		
		tx().lock(resource); 

		final List<IValueChange<?>> startChanges = action.getStartChanges();
		for (IValueChange<?> change : startChanges) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change);
		}

		final List<IValueChange<?>> endChanges = action.getEndChanges();
		for (IValueChange<?> change : endChanges) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change);
		}
		// finally set the action state
		action.setState(State.PLANNED);
		action.setResourceId(resource.getId());
	}

	@Override
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void undo() {

		Locator locator = Locator.newBuilder(Tags.RESOURCE, action.getResourceType(), action.getResourceId()).build();
		Resource resource = tx().findElement(locator);

		final List<IValueChange<?>> startChanges = action.getStartChanges();
		for (IValueChange<?> change : startChanges) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change.getInverse());
		}

		final List<IValueChange<?>> endChanges = action.getEndChanges();
		for (IValueChange<?> change : endChanges) {
			final StrolchTimedState timedState = resource.getTimedState(change.getStateId());
			timedState.applyChange(change.getInverse());
		}
		// finally set the action state
		action.setState(State.CREATED);
	}

	public void setAction(Action action) {
		this.action = action;
	}

}
