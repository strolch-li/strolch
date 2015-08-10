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

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ACTIVITY_REF;

import java.util.List;

import li.strolch.agent.api.ActivityMap;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.model.query.ActivityQuery;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.persistence.inmemory.InMemoryActivityDao;

public class CachedActivityMap extends CachedElementMap<Activity> implements ActivityMap {

	private ActivityDao cachedDao;

	public CachedActivityMap() {
		super();
		this.cachedDao = new InMemoryActivityDao();
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);
	}

	@Override
	protected ActivityDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getActivityDao(tx);
	}

	@Override
	public ActivityDao getCachedDao() {
		return this.cachedDao;
	}

	@Override
	public <U> List<U> doQuery(StrolchTransaction tx, ActivityQuery<U> query) {
		return getCachedDao().doQuery(query);
	}
}
