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
package li.strolch.service;

import java.util.HashSet;
import java.util.Set;

import li.strolch.service.api.ServiceArgument;

public class XmlImportModelArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;

	public String modelFileName;
	public boolean external = false;
	public boolean allowInclude = true;
	public boolean addOrders = true;
	public boolean addResources = true;
	public boolean addActivities = true;
	public boolean updateOrders = true;
	public boolean updateResources = true;
	public boolean updateActivities = true;
	public Set<String> orderTypes = new HashSet<>();
	public Set<String> resourceTypes = new HashSet<>();
	public Set<String> activityTypes = new HashSet<>();

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("XmlImportModelArgument [ ");

		builder.append("external=");
		builder.append(this.external);

		builder.append(", allowInclude=");
		builder.append(this.allowInclude);

		builder.append(", modelFileName=");
		builder.append(this.modelFileName);

		if (this.addOrders)
			builder.append(", addOrders");
		if (this.addResources)
			builder.append(", addResources");
		if (this.addActivities)
			builder.append(", addActivities");

		if (this.updateOrders)
			builder.append(", updateOrders");
		if (this.updateResources)
			builder.append(", updateResources");
		if (this.updateActivities)
			builder.append(", updateActivities");

		if (this.resourceTypes != null && !this.resourceTypes.isEmpty()) {
			builder.append(", resourceTypes=");
			builder.append(this.resourceTypes);
		} else {
			builder.append(", resourceTypes=*");
		}

		if (this.orderTypes != null && !this.orderTypes.isEmpty()) {
			builder.append(", orderTypes=");
			builder.append(this.orderTypes);
		} else {
			builder.append(", orderTypes=*");
		}

		if (this.activityTypes != null && !this.activityTypes.isEmpty()) {
			builder.append(", activityTypes=");
			builder.append(this.activityTypes);
		} else {
			builder.append(", activityTypes=*");
		}

		builder.append("]");
		return builder.toString();
	}
}