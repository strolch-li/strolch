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

import ch.eitchnet.utils.dbc.DBC;
import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.AuditTrail;
import li.strolch.agent.api.ElementMap;
import li.strolch.model.ActivityVisitor;
import li.strolch.model.activity.Activity;
import li.strolch.model.query.ActivityQuery;
import li.strolch.persistence.api.StrolchTransaction;

/**
 * This is the {@link AuditTrail} for {@link Activity Activities}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @see AuditingElementMapFacade
 */
public class AuditingActivityMap extends AuditingElementMapFacade<Activity> implements ActivityMap {

	/**
	 * @param elementMap
	 */
	public AuditingActivityMap(ElementMap<Activity> elementMap, boolean observeAccessReads) {
		super(elementMap, observeAccessReads);
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
			return activityVisitor.visit(activity);
		});

		return getElementMap().doQuery(tx, query);
	}
}
