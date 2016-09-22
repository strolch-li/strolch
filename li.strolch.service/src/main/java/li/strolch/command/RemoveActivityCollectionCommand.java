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
package li.strolch.command;

import java.text.MessageFormat;
import java.util.List;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.ComponentContainer;
import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveActivityCollectionCommand extends Command {

	private List<Activity> activities;

	/**
	 * @param tx
	 */
	public RemoveActivityCollectionCommand(ComponentContainer container, StrolchTransaction tx) {
		super(container, tx);
	}

	/**
	 * @param activities
	 *            the activities to set
	 */
	public void setActivities(List<Activity> activities) {
		this.activities = activities;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Activity list may not be null!", this.activities);
	}

	@Override
	public void doCommand() {

		for (Activity activity : this.activities) {
			tx().lock(activity);
		}

		ActivityMap activityMap = tx().getActivityMap();
		for (Activity activity : this.activities) {
			if (!activityMap.hasElement(tx(), activity.getType(), activity.getId())) {
				String msg = "The Activity {0} can not be removed as it does not exist!";
				msg = MessageFormat.format(msg, activity.getLocator());
				throw new StrolchException(msg);
			}
		}

		activityMap.removeAll(tx(), this.activities);
	}

	@Override
	public void undo() {
		if (this.activities != null && !this.activities.isEmpty() && tx().isRollingBack()) {
			ActivityMap activityMap = tx().getActivityMap();
			for (Activity activity : this.activities) {
				if (!activityMap.hasElement(tx(), activity.getType(), activity.getId())) {
					activityMap.add(tx(), activity);
				}
			}
		}
	}
}
