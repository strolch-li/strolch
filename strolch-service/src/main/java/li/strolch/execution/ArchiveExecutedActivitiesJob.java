/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.execution;

import li.strolch.agent.api.StrolchAgent;
import li.strolch.execution.command.ArchiveActivityCommand;
import li.strolch.job.JobMode;
import li.strolch.job.StrolchJob;
import li.strolch.model.State;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.model.PrivilegeContext;

import java.util.concurrent.TimeUnit;

public class ArchiveExecutedActivitiesJob extends StrolchJob {

	public ArchiveExecutedActivitiesJob(StrolchAgent agent, String id, String name, JobMode jobMode) {
		super(agent, id, name, jobMode);
	}

	public ArchiveExecutedActivitiesJob(StrolchAgent agent, JobMode jobMode, long initialDelay,
			TimeUnit initialDelayTimeUnit, long delay, TimeUnit delayTimeUnit) {
		super(agent, ArchiveExecutedActivitiesJob.class.getSimpleName(),
				ArchiveExecutedActivitiesJob.class.getSimpleName(), jobMode);
		setDelay(initialDelay, initialDelayTimeUnit, delay, delayTimeUnit);
	}

	@Override
	protected void execute(PrivilegeContext ctx) {

		try (StrolchTransaction tx = openTx(ctx.getCertificate())) {
			tx.streamActivities().forEach(activity -> {
				if (activity.getState() == State.EXECUTED) {
					ArchiveActivityCommand command = new ArchiveActivityCommand(tx);
					command.setActivityLoc(activity.getLocator());
					tx.addCommand(command);
				}
			});

			tx.commitOnClose();
		}
	}
}
