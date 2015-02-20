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

public class XmlExportModelArgument extends ServiceArgument {
	private static final long serialVersionUID = 1L;

	public String modelFileName;
	public boolean external;
	public boolean overwrite;
	public boolean multiFile;
	public boolean doOrders = true;
	public boolean doResources = true;
	public Set<String> orderTypes = new HashSet<>();
	public Set<String> resourceTypes = new HashSet<>();

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("XmlExportModelArgument [ ");

		builder.append("external=");
		builder.append(this.external);

		builder.append("modelFileName=");
		builder.append(this.modelFileName);

		builder.append(", overwrite=");
		builder.append(this.overwrite);
		builder.append(", multiFile=");
		builder.append(this.multiFile);

		if (this.doResources)
			builder.append(", resources");
		if (this.doOrders)
			builder.append(", orders");

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

		builder.append("]");
		return builder.toString();
	}
}