/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.agent.api.*;
import li.strolch.model.Resource;
import li.strolch.model.Order;
import li.strolch.model.activity.Activity;
import li.strolch.persistence.api.StrolchTransaction;

import java.time.ZonedDateTime;
import java.time.ZoneId;
import java.util.concurrent.locks.Lock;

import static li.strolch.model.StrolchModelConstants.*;

/** Owns the sparse views for one transaction, not a snapshot or a realm-wide validation revision. */
public final class TransactionMaps {
	private final InternalStrolchRealm realm;
	private ResourceView resources;
	private OrderView orders;
	private ActivityView activities;
	private Runnable preparedPublication;
	private ModelPublication.AffectedRoots preparedAffected;

	public TransactionMaps(InternalStrolchRealm realm) { this(realm, false); }

	TransactionMaps(InternalStrolchRealm realm, boolean readOnly) {
		this.realm = realm;
		if (readOnly) {
			this.resources = new ResourceView((TransientElementMap<Resource>) realm.getResourceMap(), null);
			this.orders = new OrderView((TransientElementMap<Order>) realm.getOrderMap(), null);
			this.activities = new ActivityView((TransientElementMap<Activity>) realm.getActivityMap(), null);
		}
	}

	public static boolean supported(InternalStrolchRealm realm) {
		return realm.getMode() == DataStoreMode.TRANSIENT || realm.getMode() == DataStoreMode.CACHED || realm.getMode() == DataStoreMode.EMPTY;
	}

	public ResourceMap resources(StrolchTransaction tx) {
		if (this.resources == null)
			this.resources = new ResourceView((TransientElementMap<Resource>) this.realm.getResourceMap(), tx);
		return this.resources;
	}
	public OrderMap orders(StrolchTransaction tx) {
		if (this.orders == null)
			this.orders = new OrderView((TransientElementMap<Order>) this.realm.getOrderMap(), tx);
		return this.orders;
	}
	public ActivityMap activities(StrolchTransaction tx) {
		if (this.activities == null)
			this.activities = new ActivityView((TransientElementMap<Activity>) this.realm.getActivityMap(), tx);
		return this.activities;
	}
	public boolean hasChanges() {
		return (this.resources != null && this.resources.hasChanges()) || (this.orders != null && this.orders.hasChanges()) ||
				(this.activities != null && this.activities.hasChanges());
	}
	public Lock persistenceLock() {
		if (this.realm.getMode() != DataStoreMode.CACHED) return null;
		boolean bulk = (this.resources != null && this.resources.hasBulkClear()) || (this.orders != null && this.orders.hasBulkClear()) ||
				(this.activities != null && this.activities.hasBulkClear());
		return this.realm.getModelPublication().persistenceLock(bulk);
	}
	private boolean affects(String interpretation, String type, String id) {
		return switch (interpretation) {
			case INTERPRETATION_RESOURCE_REF -> this.resources != null && this.resources.affects(type, id);
			case INTERPRETATION_ORDER_REF -> this.orders != null && this.orders.affects(type, id);
			case INTERPRETATION_ACTIVITY_REF -> this.activities != null && this.activities.affects(type, id);
			default -> throw new IllegalArgumentException(interpretation);
		};
	}

	public void preparePersistence() {
		this.preparedAffected = this::affects;
		this.preparedPublication = () -> {
			if (this.resources != null && this.resources.hasChanges()) this.resources.publish();
			if (this.orders != null && this.orders.hasChanges()) this.orders.publish();
			if (this.activities != null && this.activities.hasChanges()) this.activities.publish();
		};
		this.realm.getModelPublication().assertAvailable();
		if (this.resources != null && this.resources.hasChanges()) this.resources.preparePersistence();
		if (this.orders != null && this.orders.hasChanges()) this.orders.preparePersistence();
		if (this.activities != null && this.activities.hasChanges()) this.activities.preparePersistence();
	}
	public void publish() {
		if (!hasChanges()) return;
		this.realm.getModelPublication().publish(this.preparedPublication, this.preparedAffected);
	}

	private final class ResourceView extends TransactionElementMap<Resource> implements ResourceMap {
		ResourceView(TransientElementMap<Resource> source, StrolchTransaction tx) {
			super(source, tx, realm.getModelPublication(), INTERPRETATION_RESOURCE_REF);
		}
	}
	private final class OrderView extends TransactionElementMap<Order> implements OrderMap {
		OrderView(TransientElementMap<Order> source, StrolchTransaction tx) {
			super(source, tx, realm.getModelPublication(), INTERPRETATION_ORDER_REF);
		}
		@Override
		public Order getTemplate(StrolchTransaction tx, String type, boolean assertExists) {
			Order order = super.getTemplate(tx, type, assertExists);
			if (order != null) order.setDate(ZonedDateTime.now(ZoneId.systemDefault()));
			return order;
		}
	}
	private final class ActivityView extends TransactionElementMap<Activity> implements ActivityMap {
		ActivityView(TransientElementMap<Activity> source, StrolchTransaction tx) {
			super(source, tx, realm.getModelPublication(), INTERPRETATION_ACTIVITY_REF);
		}
	}
}
