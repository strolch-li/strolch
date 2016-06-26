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

import java.util.Iterator;
import java.util.Map.Entry;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.model.Resource;
import li.strolch.model.activity.Action;
import li.strolch.model.activity.Activity;
import li.strolch.model.activity.IActivityElement;
import li.strolch.model.timedstate.StrolchTimedState;
import li.strolch.model.timevalue.IValueChange;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * Command to plan an {@link Activity} to a {@link Resource}. This
 * {@link Command} assumes that the {@link IValueChange} objects of the action
 * are already constructed and {@link Action#resourceId} is set.
 * <p/>
 * It iterates the {@link IValueChange} operators and registers the resulting
 * changes on the {@link StrolchTimedState} objects assigned to the
 * {@link Resource}.
 * 
 * @author Martin Smock <martin.smock@bluewin.ch>
 */
public class PlanActivityCommand extends AbstractPlanCommand {

	protected Activity activity;

	/**
	 * @param container
	 * @param tx
	 */
	public PlanActivityCommand(final ComponentContainer container, final StrolchTransaction tx) {
		super(container, tx);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Activity may not be null!", this.activity);
		validate(activity);
	}

	private void validate(final Action action) {
		DBC.PRE.assertNotNull("Action attribute resourceId may not be null!", action.getResourceId());
		DBC.PRE.assertNotNull("Action attribute resourceType may not be null!", action.getResourceType());
	}

	private void validate(final Activity activity) {
		final Iterator<Entry<String, IActivityElement>> elementIterator = activity.elementIterator();
		while (elementIterator.hasNext()) {
			final IActivityElement activityElement = elementIterator.next().getValue();
			if (activityElement instanceof Activity)
				validate((Activity) activityElement);
			else if (activityElement instanceof Action)
				validate((Action) activityElement);
		}
	}

	@Override
	public void doCommand() {
		validate();
		tx().lock(activity);
		plan(activity);
	}

	@Override
	public void undo() {
		tx().lock(activity);
		unplan(activity);
	}

	public void setActivity(Activity activity) {
		this.activity = activity;
	}

}
