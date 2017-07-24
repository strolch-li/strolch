/*
 * Copyright 2015 Robert von Burg <eitch@eitchnet.ch>
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

import java.util.List;

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.model.Tags;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.model.visitor.ActivityVisitor;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.privilege.PrivilegeHandler;
import li.strolch.utils.dbc.DBC;

/**
 * This is the {@link AuditTrail} for {@link Activity Activities}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @see AuditingElementMapFacade
 */
public class AuditingActivityMap extends AuditingElementMapFacade<Activity> implements ActivityMap {

	public AuditingActivityMap(PrivilegeHandler privilegeHandler, ElementMap<Activity> elementMap,
			boolean observeAccessReads) {
		super(privilegeHandler, elementMap, observeAccessReads);
	}

	@Override
	protected String getElementType() {
		return Tags.ACTIVITY;
	}

	@Override
	protected ActivityMap getElementMap() {
		return (ActivityMap) super.getElementMap();
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ActivityQuery<U> query) {
		ActivityVisitor<U> activityVisitor = query.getActivityVisitor();
		DBC.PRE.assertNotNull("activityVisitor on query", activityVisitor);
		query.setActivityVisitor(activity -> {
			this.read.add(activity);
			return activity.accept(activityVisitor);
		});

		return getElementMap().doQuery(tx, query);
	}
}
