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

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.OrderMap;
import li.strolch.agent.api.ResourceMap;
import li.strolch.model.ModelStatistics;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.service.api.Command;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ClearModelCommand extends Command {

	// input
	private boolean clearOrders;
	private boolean clearResources;
	private boolean clearActivities;

	// output
	private ModelStatistics statistics;

	/**
	 * @param container
	 * @param tx
	 */
	public ClearModelCommand(StrolchTransaction tx) {
		super(tx);
	}

	@Override
	public void validate() {
		// nothing to do
	}

	@Override
	public void doCommand() {

		ModelStatistics statistics = new ModelStatistics();

		if (this.clearOrders) {
			OrderMap orderMap = tx().getOrderMap();
			statistics.nrOfOrders = orderMap.removeAll(tx());
		}

		if (this.clearResources) {
			ResourceMap resourceMap = tx().getResourceMap();
			statistics.nrOfResources = resourceMap.removeAll(tx());
		}

		if (this.clearActivities) {
			ActivityMap activitiesMap = tx().getActivityMap();
			statistics.nrOfActivities = activitiesMap.removeAll(tx());
		}

		this.statistics = statistics;
	}

	@Override
	public void undo() {
		logger.warn("Can not undo clearing of model!");
	}

	/**
	 * @param clearOrders
	 * 		the clearOrders to set
	 */
	public void setClearOrders(boolean clearOrders) {
		this.clearOrders = clearOrders;
	}

	/**
	 * @param clearResources
	 * 		the clearResources to set
	 */
	public void setClearResources(boolean clearResources) {
		this.clearResources = clearResources;
	}

	/**
	 * @param clearActivities
	 * 		the clearActivities to set
	 */
	public void setClearActivities(boolean clearActivities) {
		this.clearActivities = clearActivities;
	}

	/**
	 * @return the statistics
	 */
	public ModelStatistics getStatistics() {
		return this.statistics;
	}
}
