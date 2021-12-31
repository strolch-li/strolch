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
package li.strolch.model;

import static li.strolch.utils.helper.StringHelper.NULL;

import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import com.google.gson.JsonObject;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class ModelStatistics {

	public LocalDateTime startTime;
	public long durationNanos;
	public long nrOfResources;
	public long nrOfResourcesUpdated;
	public long nrOfOrders;
	public long nrOfOrdersUpdated;
	public long nrOfActivities;
	public long nrOfActivitiesUpdated;

	public long getNrOfResources() {
		return this.nrOfResources;
	}

	public long getNrOfResourcesUpdated() {
		return nrOfResourcesUpdated;
	}

	public long getNrOfOrders() {
		return this.nrOfOrders;
	}

	public long getNrOfOrdersUpdated() {
		return nrOfOrdersUpdated;
	}

	public long getNrOfActivities() {
		return this.nrOfActivities;
	}

	public long getNrOfActivitiesUpdated() {
		return nrOfActivitiesUpdated;
	}

	public long getNrOfElements() {
		return this.nrOfResources + this.nrOfOrders + this.nrOfActivities;
	}

	/**
	 * Adds the statistics of the other statistics to this statistics instance
	 *
	 * @param statistics
	 * 		further statistics to add to this {@link ModelStatistics}
	 */
	public void add(ModelStatistics statistics) {
		this.nrOfResources += statistics.nrOfResources;
		this.nrOfResourcesUpdated += statistics.nrOfResourcesUpdated;
		this.nrOfOrders += statistics.nrOfOrders;
		this.nrOfOrdersUpdated += statistics.nrOfOrdersUpdated;
		this.nrOfActivities += statistics.nrOfActivities;
		this.nrOfActivitiesUpdated += statistics.nrOfActivitiesUpdated;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName());
		builder.append(",\n- startTime=");
		builder.append(this.startTime == null ?
				NULL :
				this.startTime.toLocalTime().truncatedTo(ChronoUnit.SECONDS).toString());
		builder.append(",\n- duration=");
		builder.append(StringHelper.formatNanoDuration(this.durationNanos));

		if (this.nrOfResourcesUpdated == 0)
			builder.append(",\n- Resources=").append(this.nrOfResources);
		else
			builder.append(",\n- Resources: added=").append(this.nrOfResources).append(" updated=")
					.append(this.nrOfResourcesUpdated);

		if (this.nrOfOrdersUpdated == 0)
			builder.append(",\n- Orders=").append(this.nrOfOrders);
		else
			builder.append(",\n- Orders: added=").append(this.nrOfOrders).append(" updated=")
					.append(this.nrOfOrdersUpdated);

		if (this.nrOfActivitiesUpdated == 0)
			builder.append(",\n- Activities=").append(this.nrOfActivities);
		else
			builder.append(",\n- Activities: added=").append(this.nrOfActivities).append(" updated=")
					.append(this.nrOfActivitiesUpdated);
		return builder.toString();
	}

	public JsonObject toJson() {
		JsonObject json = new JsonObject();

		json.addProperty("startTime", this.startTime == null ? NULL : ISO8601.toString(this.startTime));
		json.addProperty("durationNanos", durationNanos);
		json.addProperty("resourcesAdded", nrOfResources);
		json.addProperty("resourcesUpdated", nrOfResourcesUpdated);
		json.addProperty("ordersAdded", nrOfOrders);
		json.addProperty("ordersUpdated", nrOfOrdersUpdated);
		json.addProperty("activitiesAdded", nrOfActivities);
		json.addProperty("activitiesUpdated", nrOfActivitiesUpdated);

		return json;
	}
}