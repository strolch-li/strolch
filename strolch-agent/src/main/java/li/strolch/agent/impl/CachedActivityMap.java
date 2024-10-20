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

import li.strolch.agent.api.ActivityMap;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.activity.Activity;
import li.strolch.model.parameter.Parameter;
import li.strolch.persistence.api.ActivityDao;
import li.strolch.persistence.api.StrolchTransaction;

import static li.strolch.model.StrolchModelConstants.INTERPRETATION_ACTIVITY_REF;

public class CachedActivityMap extends CachedElementMap<Activity> implements ActivityMap {

	public CachedActivityMap(StrolchRealm realm) {
		super(realm);
	}

	@Override
	protected void assertIsRefParam(Parameter<?> refP) {
		ElementMapHelpers.assertIsRefParam(INTERPRETATION_ACTIVITY_REF, refP);
	}

	@Override
	protected ActivityDao getDbDao(StrolchTransaction tx) {
		return tx.getPersistenceHandler().getActivityDao(tx);
	}
}
