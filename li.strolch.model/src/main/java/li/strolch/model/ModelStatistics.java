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

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import li.strolch.model.xml.Iso8601DateAdapter;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.iso8601.ISO8601FormatFactory;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
@XmlRootElement(name = "ModelStatistics")
@XmlAccessorType(XmlAccessType.NONE)
public class ModelStatistics {

	@XmlAttribute(name = "startTime")
	@XmlJavaTypeAdapter(value = Iso8601DateAdapter.class)
	public Date startTime;

	@XmlAttribute(name = "duractionNanos")
	public long durationNanos;

	@XmlAttribute(name = "nrOfResources")
	public long nrOfResources;

	@XmlAttribute(name = "nrOfOrders")
	public long nrOfOrders;

	@XmlAttribute(name = "nrOfActivities")
	public long nrOfActivities;

	/**
	 * @return the nrOfOrders
	 */
	public long getNrOfOrders() {
		return this.nrOfOrders;
	}

	/**
	 * @return the nrOfResources
	 */
	public long getNrOfResources() {
		return this.nrOfResources;
	}

	/**
	 * @return the nrOfResources + nrOfOrders
	 */
	public long getNrOfElements() {
		return this.nrOfOrders + this.nrOfResources;
	}

	/**
	 * @return the nrOfActivities
	 */
	public long getNrOfActivities() {
		return this.nrOfActivities;
	}

	/**
	 * Adds the statistics of the other statistics to this statistics instance
	 * 
	 * @param statistics
	 */
	public void add(ModelStatistics statistics) {
		this.nrOfOrders += statistics.nrOfOrders;
		this.nrOfResources += statistics.nrOfResources;
		this.nrOfActivities += statistics.nrOfActivities;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append(getClass().getSimpleName() + " [startTime=");
		builder.append(this.startTime == null ? NULL : ISO8601FormatFactory.getInstance().formatDate(this.startTime));
		builder.append(", durationNanos=");
		builder.append(StringHelper.formatNanoDuration(this.durationNanos));
		builder.append(", nrOfResources=");
		builder.append(this.nrOfResources);
		builder.append(", nrOfOrders=");
		builder.append(this.nrOfOrders);
		builder.append(", nrOfActivities=");
		builder.append(this.nrOfActivities);
		builder.append("]");
		return builder.toString();
	}
}