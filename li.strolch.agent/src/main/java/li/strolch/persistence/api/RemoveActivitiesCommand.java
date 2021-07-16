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
package li.strolch.persistence.api;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import li.strolch.agent.api.ActivityMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveActivitiesCommand extends Command {

	private List<Activity> activities = new ArrayList<>();
	private final List<Activity> removed = new ArrayList<>();

	public RemoveActivitiesCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param activities
	 * 		the activities to set for removal
	 */
	public void setActivities(List<Activity> activities) {
		this.activities = activities;
	}

	/**
	 * @param activity
	 * 		the activity to add for removal
	 */
	public void addActivity(Activity activity) {
		this.activities.add(activity);
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotEmpty("Activities may not be empty!", this.activities);
	}

	@Override
	public void doCommand() {

		this.activities.forEach(activity -> tx().lock(activity));

		ActivityMap activityMap = tx().getActivityMap();

		this.activities.forEach(activity -> {
			if (!activityMap.hasElement(tx(), activity.getType(), activity.getId())) {
				String msg = "The Activity {0} can not be removed as it does not exist!!";
				msg = MessageFormat.format(msg, activity.getLocator());
				throw new StrolchException(msg);
			}

			activityMap.remove(tx(), activity);
			this.removed.add(activity);
		});
	}

	@Override
	public void undo() {
		if (tx().isRollingBack() && !this.removed.isEmpty()) {
			this.activities.forEach(activity -> {
				if (tx().isVersioningEnabled())
					tx().getActivityMap().undoVersion(tx(), activity);
				else
					tx().getActivityMap().add(tx(), activity);
			});
		}
	}
}
