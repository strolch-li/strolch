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

import li.strolch.agent.api.ActivityMap;
import li.strolch.exception.StrolchException;
import li.strolch.model.activity.Activity;
import li.strolch.service.api.Command;
import li.strolch.utils.dbc.DBC;

import java.text.MessageFormat;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class RemoveActivityCommand extends Command {

	private Activity activity;
	private boolean removed;

	/**
	 * @param tx
	 */
	public RemoveActivityCommand(StrolchTransaction tx) {
		super(tx);
	}

	/**
	 * @param activity
	 * 		the activity to set
	 */
	public void setActivity(Activity activity) {
		this.activity = activity;
	}

	@Override
	public void validate() {
		DBC.PRE.assertNotNull("Activity may not be null!", this.activity);
	}

	@Override
	public void doCommand() {

		tx().lock(this.activity);

		ActivityMap activityMap = tx().getActivityMap();
		if (!activityMap.hasElement(tx(), this.activity.getType(), this.activity.getId())) {
			String msg = "The Activity {0} can not be removed as it does not exist!";
			msg = MessageFormat.format(msg, this.activity.getLocator());
			throw new StrolchException(msg);
		}

		activityMap.remove(tx(), this.activity);
		this.removed = true;
	}

	@Override
	public void undo() {
		if (this.activity != null && tx().isRollingBack() && this.removed) {
			if (tx().isVersioningEnabled())
				tx().getActivityMap().undoVersion(tx(), this.activity);
			else
				tx().getActivityMap().add(tx(), this.activity);
		}
	}
}
