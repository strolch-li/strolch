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
package li.strolch.agent.impl;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.Set;

import li.strolch.exception.StrolchException;
import li.strolch.model.Order;
import li.strolch.model.Resource;
import li.strolch.model.activity.Activity;
import li.strolch.model.xml.StrolchElementListener;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InMemoryElementListener implements StrolchElementListener {

	private boolean addOrders;
	private boolean addResources;
	private boolean addActivities;
	private boolean updateOrders;
	private boolean updateResources;
	private boolean updateActivities;
	private Set<String> orderTypes;
	private Set<String> resourceTypes;
	private Set<String> activityTypes;

	private boolean failOnUpdate;

	private StrolchTransaction tx;

	public InMemoryElementListener(StrolchTransaction tx) {
		this.tx = tx;

		this.addResources = true;
		this.addOrders = true;
		this.addActivities = true;
		this.updateResources = true;
		this.updateOrders = true;
		this.updateActivities = true;
		this.orderTypes = Collections.emptySet();
		this.resourceTypes = Collections.emptySet();
		this.activityTypes = Collections.emptySet();
	}

	/**
	 * @param addResources
	 * 		the addResources to set
	 */
	public void setAddResources(boolean addResources) {
		this.addResources = addResources;
	}

	/**
	 * @param addOrders
	 * 		the addOrders to set
	 */
	public void setAddOrders(boolean addOrders) {
		this.addOrders = addOrders;
	}

	/**
	 * @param addActivities
	 * 		the addActivities to set
	 */
	public void setAddActivities(boolean addActivities) {
		this.addActivities = addActivities;
	}

	/**
	 * @param updateResources
	 * 		the updateResources to set
	 */
	public void setUpdateResources(boolean updateResources) {
		this.updateResources = updateResources;
	}

	/**
	 * @param updateOrders
	 * 		the updateOrders to set
	 */
	public void setUpdateOrders(boolean updateOrders) {
		this.updateOrders = updateOrders;
	}

	/**
	 * @param updateActivities
	 * 		the updateActivities to set
	 */
	public void setUpdateActivities(boolean updateActivities) {
		this.updateActivities = updateActivities;
	}

	/**
	 * @param orderTypes
	 * 		the orderTypes to set
	 */
	public void setOrderTypes(Set<String> orderTypes) {
		this.orderTypes = orderTypes;
	}

	/**
	 * @param resourceTypes
	 * 		the resourceTypes to set
	 */
	public void setResourceTypes(Set<String> resourceTypes) {
		this.resourceTypes = resourceTypes;
	}

	/**
	 * @param activityTypes
	 * 		the activityTypes to set
	 */
	public void setActivityTypes(Set<String> activityTypes) {
		this.activityTypes = activityTypes;
	}

	/**
	 * @param failOnUpdate
	 * 		the failOnUpdate to set
	 */
	public void setFailOnUpdate(boolean failOnUpdate) {
		this.failOnUpdate = failOnUpdate;
	}

	@Override
	public void notifyResource(Resource resource) {
		if (!this.resourceTypes.isEmpty() && !this.resourceTypes.contains(resource.getType()))
			return;

		if (this.tx.hasResource(resource.getType(), resource.getId())) {
			if (this.updateResources) {
				// we need to update the version, thus we set the current version
				Resource current = this.tx.getResourceBy(resource.getType(), resource.getId());
				resource.setVersion(current.getVersion());
				this.tx.update(resource);
			} else if (this.failOnUpdate) {
				throw new StrolchException(MessageFormat
						.format("Resource {0} already exists and updating is disallowed!", resource.getLocator()));
			}
		} else if (this.addResources) {
			this.tx.add(resource);
		}
		// else ignore
	}

	@Override
	public void notifyOrder(Order order) {
		if (!this.orderTypes.isEmpty() && !this.orderTypes.contains(order.getType()))
			return;

		if (this.tx.hasOrder(order.getType(), order.getId())) {
			if (this.updateOrders) {
				// we need to update the version, thus we set the current version
				Order current = this.tx.getOrderBy(order.getType(), order.getId());
				order.setVersion(current.getVersion());
				this.tx.update(order);
			} else if (failOnUpdate) {
				throw new StrolchException(MessageFormat
						.format("Order {0} already exists and updating is disallowed!", order.getLocator()));
			}
		} else if (this.addOrders) {
			this.tx.add(order);
		}
		// else ignore
	}

	@Override
	public void notifyActivity(Activity activity) {
		if (!this.activityTypes.isEmpty() && !this.activityTypes.contains(activity.getType()))
			return;

		if (this.tx.hasActivity(activity.getType(), activity.getId())) {
			if (this.updateActivities) {
				// we need to update the version, thus we set the current version
				Activity current = this.tx.getActivityBy(activity.getType(), activity.getId());
				activity.setVersion(current.getVersion());
				this.tx.update(activity);
			} else if (failOnUpdate) {
				throw new StrolchException(MessageFormat
						.format("Activity {0} already exists and updating is disallowed!", activity.getLocator()));
			}
		} else if (this.addActivities) {
			this.tx.add(activity);
		}
		// else ignore
	}
}
